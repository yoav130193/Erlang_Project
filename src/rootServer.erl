%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2020 11:55
%%%-------------------------------------------------------------------
-module(rootServer).
-author("yoavlevy").
-behavior(gen_server).
-include("include/header.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, terminate/2, start_link/1]).


start_link({RootCount, Mop}) ->
  gen_server:start_monitor({local, list_to_atom("root_server" ++ integer_to_list(RootCount))}, ?ROOT_SERVER, [{RootCount, Mop}], []);
start_link({RootCount, OldVersion, Mop}) ->
  gen_server:start_monitor({local, list_to_atom("root_server" ++ integer_to_list(RootCount))}, ?ROOT_SERVER, [{RootCount, Mop, OldVersion}], []).

% INIT a new Root
init([{RootCount, Mop}]) ->
  io:format("new root number: ~p Mop: ~p~n ", [RootCount, Mop]),
  put(?MY_DODAGs, [self()]),
  Version = 0,
  process_flag(trap_exit, true),
  {ok, {RootCount, Version, Mop}};

% INIT restore an existing Root
init([{RootCount, Mop, OldVersion}]) ->
  io:format("restore root number: ~p Mop: ~p~n ", [RootCount, Mop]),
  put(?MY_DODAGs, [self()]),
  process_flag(trap_exit, true),
  {ok, {RootCount, OldVersion, Mop}}.

%****************     RPL PROTOCOL MESSAGES     *****************%

% Got message from rplServer -> start to build the network
handle_cast({buildNetwork}, {RootCount, Version, Mop}) ->
  NodeList = ets:tab2list(nodeList),
  case hd(Mop) of
    ?NON_STORING ->
      put(?DOWNWARD_DIGRAPH, utils:buildVertexDigraph(NodeList));
    ?STORING ->
      io:format("STORING 87 DODAG_ID: ~p~n", [self()]),
      ets:insert(?DOWNWARD_DIGRAPH, {self(), utils:buildVertexDigraph(NodeList)})
  end,
  Rank = 0,
  put({?VERSION_RANK, self()}, {Version + 1, Rank}), % {Version, Rank}
  {MyNode, Neighbors} = utils:findMeAndNeighbors(self()), %Find My Process and My Neighbors Process
  io:format("root number: ~p start building network,~nI am : ~p~n~n", [RootCount, MyNode]),
  rpl_msg:sendDioToNeighbors(self(), self(), Rank, Version + 1, Mop, Neighbors),
  {noreply, {RootCount, Version + 1, Mop}};


% Got DIO Message - check if update is Relevant
handle_cast({dioMsg, From, DioMsg}, State) ->
  rpl_msg:handleDioMsg(DioMsg, From, State),
  {noreply, State};

%TODO - HALF implemented.. Think if need to send to an other root message
%Got Dao message from a node that got DIO -> root needs to send dao-ack back, update the childrenList and upward digraph
handle_cast({daoMsg, From, DaoMsg}, State) ->
  rpl_msg:sendDaoAckAfterDao(self(), From, DaoMsg#daoMsg.dodagId, DaoMsg#daoMsg.updateType, State),
  {noreply, State};

% Got From ACK on his DAO message, Distribute the network
handle_cast({daoAckMsg, From, DaoAckMsg}, {RootCount, Version, Mop}) ->
  rpl_msg:handleDaoAck({From, DaoAckMsg}, {RootCount, Version, Mop}),
  {noreply, {RootCount, Version, Mop}};

%*****************    DIGRAPH BUILD     *****************%

% 3 Messages for building the downward Digraph: downwardDigraphBuild, giveParent, requestParent.

% rplServer called this function, Each Root starts to build the digraph
handle_cast({downwardDigraphBuild}, {RootCount, Version, Mop}) ->
  NodeList = ets:tab2list(nodeList),
  io:format("root number: ~p Starts Building Downward Digraph~n~n", [RootCount]),
  utils:requestParent(self(), self(), NodeList),
  {noreply, {RootCount, Version, Mop}};


% Got a message that requested a parent in the specific DODAG
% return the parent if exist
handle_cast({requestParent, From, DodagID}, {RootCount, Version, Mop}) ->
  case get({?PARENT, DodagID}) of
    undefined -> % NEED TO UPDATE
      utils:deleteMessageFromEts(DodagID, From, self(), {finishedDigraphBuilding}, requestParentRoot);
    ParentList ->
      gen_server:cast(From, {giveParent, DodagID, self(), ParentList})
  end,
  {noreply, {RootCount, Version, Mop}};

% Some node gave his parent, now we can build the edge between them
handle_cast({giveParent, DodagID, From, ParentList}, {RootCount, Version, Mop}) ->
  case hd(Mop) of
    ?NON_STORING ->
      DownwardDigraph = get(?DOWNWARD_DIGRAPH),
      NewDownwardDigraph = addParentsList(DownwardDigraph, From, ParentList, DodagID),
      utils:deleteMessageFromEts(DodagID, self(), From, {finishedDigraphBuilding}, giveParentRoot),
      put(?DOWNWARD_DIGRAPH, NewDownwardDigraph);
    ?STORING ->
      {_, DownwardDigraph} = hd(ets:lookup(?DOWNWARD_DIGRAPH, self())),  % Table = ?DOWNWARD_DIGRAPH , Key = DodagId
      NewDownwardDigraph = addParentsList(DownwardDigraph, From, ParentList, DodagID),
      utils:deleteMessageFromEts(DodagID, self(), From, {finishedDigraphBuilding}, giveParentRoot),
      ets:insert(?DOWNWARD_DIGRAPH, {self(), NewDownwardDigraph})
  end,

  {noreply, {RootCount, Version, Mop}};

%*****************    SENDING A MESSAGE - CAST     *****************%

handle_cast({parentMsg, From, To, DodagID, Msg, PathToRoot}, {RootCount, Version, Mop}) ->
  MyPid = self(),
  case DodagID of
    MyPid ->
      if
        To =:= MyPid ->
          io:format("Got the Msg ! ! ! DodagID: ~p myNode: ~p msg: ~p, From: ~p, To: ~p Path:~p ~n", [DodagID, self(), Msg, From, To, PathToRoot ++ [self()]]),
          gen_server:reply(element(2, hd(ets:lookup(?RPL_REF, ref))), PathToRoot ++ [self()]);
        true ->
          io:format("parentMsg - root, DodagID: ~p myNode: ~p msg: ~p, From: ~p, To: ~p, PathToRoot: ~p~n", [DodagID, self(), Msg, From, To, PathToRoot]),
          utils:startSendMessageDownward(From, To, Msg, DodagID, PathToRoot, Mop)
      end;
    _ -> gen_server:cast(hd(get({?PARENT, DodagID})), {parentMsg, From, To, DodagID, Msg, PathToRoot ++ [self()]})
  end,
  {noreply, {RootCount, Version, Mop}};

handle_cast({downwardMessage, From, To, Msg, DodagID, PathList, WholePath}, State) ->
  utils:handleDownwardMessage(DodagID, Msg, From, To, WholePath, PathList),
  {noreply, State};


%*****************    DEBUG FUNCTIONS     *****************%


handle_cast({getAllPath}, {RootCount, Version, Mop}) ->
  NodeList = ets:tab2list(nodeList),
  getAllPath(NodeList, Mop),
  {noreply, {RootCount, Version, Mop}}.

%*****************    SENDING A MESSAGE - CALL     *****************%

handle_call({calculateRoute, To}, From, {RootCount, Version, Mop}) ->
  {Path, PathLength} = utils:calculatePath(To, Mop, self()),
  io:format("calculateRoute, DODAGID: ~p, Path: ~pPathLength: ~p~n", [self(), Path, PathLength]),
  {reply, {Path, PathLength}, {RootCount, Version, Mop}};

handle_call({hi}, From, State) ->
  io:format("Got hi: ~p~n", [self()]),
  {reply, cool, State};

handle_call({sendMessageStoring, From, To, Msg}, OrderFrom, {RootCount, Version, Mop}) ->
  ets:insert(?RPL_REF, {ref, OrderFrom}),
  io:format("OrderFrom: ~p~n", [OrderFrom]),
  utils:sendMessageStoring(From, To, Msg, Mop),
  {noreply, {RootCount, Version, Mop}};


handle_call({sendMessageNonStoring, From, To, Msg}, OrderFrom, {RootCount, Version, Mop}) ->
  ets:insert(?RPL_REF, {ref, OrderFrom}),
  io:format("OrderFrom: ~p~n", [OrderFrom]),
  utils:sendMessageNonStoring(From, To, Msg, Mop),
  {noreply, {RootCount, Version, Mop}};

handle_call(Request, From, State) ->
  erlang:error(not_implemented).


%************     DEBUG FUNCTION    *************%


% Prints each node all it's edges
printAllEdges(NodeList) ->
  {ok, S} = file:open(?DOWNWARD_DIGRAPH_FILE, [append]),
  lists:foreach(fun(Element) ->
    Edges = digraph:edges(get(?DOWNWARD_DIGRAPH), element(1, Element)),
    io:format(S, "DODAG_ID: ~p, Vertix: ~p, Edges: ~p~n", [self(), element(1, Element), Edges])
                end, NodeList).

% GET ALL THE PATH FROM EACH NODE TO EACH NODE
getAllPath(NodeList, Mop) ->
  {ok, S} = file:open(?DOWNWARD_DIGRAPH_FILE, [append]),
  lists:foreach(fun(OutElement) ->
    lists:foreach(fun(InElement) ->
      case hd(Mop) of
        ?STORING ->
          {_, DownwardDigraph} = hd(ets:lookup(?DOWNWARD_DIGRAPH, self())),
          Vertices = digraph:get_short_path(DownwardDigraph, element(1, OutElement), element(1, InElement));
        ?NON_STORING ->
          Vertices = digraph:get_short_path(get(?DOWNWARD_DIGRAPH), element(1, OutElement), element(1, InElement))
      end,
      io:format(S, "DODAG_ID: ~p, From: ~p, TO: ~p, Path: ~p~n",
        [self(), element(1, OutElement), element(1, InElement), Vertices])
                  end, NodeList)
                end, NodeList).


addParentsList(DownwardDigraph, From, ParentList, DodagID) ->
  lists:foreach(fun(Parent) ->
    io:format("add_edge -DodagID: ~p, Me: ~p,  Parent: ~p Child: ~p~n", [DodagID, self(), Parent, From]),
    digraph:add_edge(DownwardDigraph, Parent, From),
    digraph:add_edge(DownwardDigraph, From, Parent)
                end, ParentList),
  DownwardDigraph.




terminate(Reason, State) ->
  io:format("rootServer: ~p, terminate , Reason:~p, State: ~p~n", [self(), Reason, State]),
  ets:delete(?NODE_LIST, self()),
  ets:delete(?ROOT_LIST, self()),
  script:checkLists(),
  exit({rootCrash, Reason, State}).