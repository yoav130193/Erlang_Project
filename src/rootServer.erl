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
%% API
-export([init/1, handle_call/3, handle_cast/2, terminate/2, start_link/1]).


-record(dioMsg, {rplInstanceId, versionNumber, rank, g = 2#1, zero = 2#0, mop, prf = 2#000, dtsn, flags = 16#00, reserved = 16#00, dodagId}).
-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId}).
-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId}).

-define(VERSION_RANK, version_rank).
-define(PARENT, parent).
-define(DOWNWARD_DIGRAPH, downwardDigraph).
-define(DOWNWARD_DIGRAPH_FILE, "downward_digraph_file.txt").
-define(ROOT_SERVER, rootServer).

start_link(RootCount) -> io:format("new root number: ~p~n ", [list_to_atom("root_server" ++ "1")]),

  gen_server:start_link({local, list_to_atom("root_server" ++ integer_to_list(RootCount))}, ?ROOT_SERVER, [RootCount], []).

init(RootCount) ->
  io:format("new root number: ~p~n ", [RootCount]),
  Version = 0,
  Mop = element(2, hd(ets:lookup(mop, mopKey))),
  {ok, {RootCount, Version, Mop}}.


% Got message from rplServer -> start to build the network
handle_cast({buildNetwork}, {RootCount, Version, Mop}) ->
  Rank = 0,
  put({?VERSION_RANK, self()}, {Version + 1, Rank}), % {Version, Rank}
  {MyNode, Neighbors} = utils:findMeAndNeighbors(self()), %Find My Process and My Neighbors Process
  io:format("root number: ~p start building network,~n From : ~p~n~n", [RootCount, MyNode]),
  rpl_msg:sendDioToNeighbors(self(), self(), Rank, Version + 1, Mop, Neighbors),
  {noreply, {RootCount, Version + 1, Mop}};

% Got DIO Message - check if update is Relevant
handle_cast({dioMsg, From, DioMsg}, State) ->
  UpdateNeeded = utils:checkIfUpdateNeeded(DioMsg#dioMsg.dodagId, DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1, From),
  if
    UpdateNeeded =:= true -> % Update rank and version -> send back DAO message
      put({?VERSION_RANK, DioMsg#dioMsg.dodagId}, {DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1}),
      rpl_msg:sendDaoAfterDio(self(), From, DioMsg#dioMsg.dodagId, State);
    true -> rpl_msg:noNeedToUpdateToFile(self(), From, DioMsg#dioMsg.dodagId),
      {noreply, State}
  end;

% Got From ACK on his DAO message, Distribute the network
handle_cast({daoAckMsg, From, DaoAckMsg}, {RootCount, Version, Mop}) ->
  put({?PARENT, DaoAckMsg#daoAckMsg.dodagId}, From), % Update Parent
  {Version, Rank} = get({?VERSION_RANK, DaoAckMsg#daoAckMsg.dodagId}),
  {MyNode, Neighbors} = utils:findMeAndNeighbors(self()),
  io:format("node number: ~p Continue To Build,~n From : ~p~n~n", [RootCount, MyNode]),
  rpl_msg:sendDioToNeighbors(self(), DaoAckMsg#daoAckMsg.dodagId, Rank, Version, Mop, Neighbors),
  {noreply, {RootCount, Version, Mop}};

% 3 Messages for building the downward Digraph: downwardDigraphBuild, giveParent, requestParent.
handle_cast({downwardDigraphBuild}, {RootCount, Version, Mop}) ->
  NodeList = ets:tab2list(nodeList),
  put(?DOWNWARD_DIGRAPH, utils:buildVertexDigraph(NodeList)),
  io:format("root number: ~p Starts Building Downward Digraph~n~n", [RootCount]),
% FROM,DODOAG, NodeList
  utils:requestParent(self(), self(), NodeList),
  {noreply, {RootCount, Version, Mop}};


% Got a message that requested a parent in the specific DODAG
% return the parent if exist
handle_cast({requestParent, From, DodagID}, {RootCount, Version, Mop}) ->
  case get({?PARENT, DodagID}) of
    undefined -> % NEED TO UPDATE
      continue;
    Parent ->
      gen_server:cast(From, {giveParent, DodagID, self(), Parent})
  end,
  {noreply, {RootCount, Version, Mop}};

handle_cast({giveParent, DodagID, From, Parent}, {RootCount, Version, Mop}) ->
  io:format("giveParent, DodagID: ~p myNode: ~p Child: ~p Parent: ~p~n", [DodagID, self(), From, Parent]),
  DownwardDigraph = get(?DOWNWARD_DIGRAPH),
  digraph:add_edge(DownwardDigraph, Parent, From),
  put(?DOWNWARD_DIGRAPH, DownwardDigraph),
  {noreply, {RootCount, Version, Mop}};

handle_cast({sendMessage, {From, To, Msg}}, {RootCount, Version, Mop}) ->
  %TODO - implement this
  {noreply, {RootCount, Version, Mop}};

handle_cast({getAllPath}, State) ->
  NodeList = ets:tab2list(nodeList),
  getAllPath(NodeList),
  {noreply, State};


%TODO - HALF implemented.. Think if need to send to an other root message
%Got Dao message from a node that got DIO -> root needs to send dao-ack back, update the childrenList and upward digraph
handle_cast({daoMsg, From, DaoMsg}, State) ->
  rpl_msg:sendDaoAckAfterDao(self(), From, DaoMsg#daoMsg.dodagId, State),
  {noreply, State}.


terminate(_Reason, _State) ->
  io:format("rootServer terminate~n"),
  [].


%************     DEBUG FUNCTION    *************%


% Prints each node all it's edges
printAllEdges(NodeList) ->
  {ok, S} = file:open(?DOWNWARD_DIGRAPH_FILE, [append]),
  lists:foreach(fun(Element) ->
    Edges = digraph:edges(get(?DOWNWARD_DIGRAPH), element(1, Element)),
    io:format(S, "DODAG_ID: ~p , Vertix: ~p,  Edges: ~p~n", [self(), element(1, Element), Edges])
                end, NodeList).

% GET ALL THE PATH FROM EACH NODE TO EACH NODE
getAllPath(NodeList) ->
  {ok, S} = file:open(?DOWNWARD_DIGRAPH_FILE, [append]),
  lists:foreach(fun(OutElement) ->
    lists:foreach(fun(InElement) ->
      Vertices = digraph:get_path(get(?DOWNWARD_DIGRAPH), element(1, OutElement), element(1, InElement)),
      io:format(S, "DODAG_ID: ~p , From: ~p, TO: ~p,  Path: ~p~n", [self(), element(1, OutElement), element(1, InElement), Vertices])
                  end, NodeList)
                end, NodeList).


handle_call(Request, From, State) ->
  erlang:error(not_implemented).