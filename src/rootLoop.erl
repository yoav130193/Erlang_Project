%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2020 11:56
%%%-------------------------------------------------------------------
-module(rootLoop).
-author("yoavlevy").

%% API
-export([loopStart/1]).

-record(dioMsg, {rplInstanceId, versionNumber, rank, g = 2#1, zero = 2#0, mop, prf = 2#000, dtsn, flags = 16#00, reserved = 16#00, dodagId}).
-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId}).
-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId}).

-define(VERSION_RANK, version_rank).
-define(PARENT, parent).
-define(DOWNWARD_DIGRAPH, downwardDigraph).
-define(DOWNWARD_DIGRAPH_FILE, "downward_digraph_file.txt").


%Dictionaries
% 1. key: {DodagID,rank_version}, value: {Version,Rank}
% 2. key: {DodagID,parent},      value: {parentPid}
% 3. key: {DodagID,children},        value: {childrenList}


loopStart(RootCount) ->
  io:format("new root number: ~p~n ", [RootCount]),
  Version = 0,
  Mop = element(2, hd(ets:lookup(mop, mopKey))),
  loop(RootCount, Version, Mop).

% Locations = list of {NodePid,X,Y}
loop(RootCount, Version, Mop) ->
  receive
    {buildNetwork} -> % Got message from myServer -> start to build the network
      Rank = 0,
      put({?VERSION_RANK, self()}, {Version + 1, Rank}), % {Version, Rank}
      {MyNode, Neighbors} = utils:findMeAndNeighbors(self()), %Find My Process and My Neighbors Process
      io:format("root number: ~p start building network,~n From : ~p~n~n", [RootCount, MyNode]),
      rpl_msg:sendDioToNeighbors(self(), self(), Rank, Version + 1, Mop, Neighbors),
      loop(RootCount, Version + 1, Mop);

  % Got DIO Message - check if update is Relevant
    {dioMsg, From, DioMsg} ->
      UpdateNeeded = utils:checkIfUpdateNeeded(DioMsg#dioMsg.dodagId, DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1, From),
      if
        UpdateNeeded =:= true -> % Update rank and version -> send back DAO message
          put({?VERSION_RANK, DioMsg#dioMsg.dodagId}, {DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1}),
          rpl_msg:sendDaoAfterDio(self(), From, DioMsg#dioMsg.dodagId);
        true -> rpl_msg:noNeedToUpdateToFile(self(), From, DioMsg#dioMsg.dodagId)
      end,
      loop(RootCount, Version, Mop);

  %TODO - HALF implemented! Think if need to send to an other root message
%Got Dao message from a node that got DIO -> root needs to send dao-ack back, update the childrenList and upward digraph
    {daoMsg, From, DaoMsg} ->
      % utils:updateUpwardDigraph(self(), From, DaoMsg),
      rpl_msg:sendDaoAckAfterDao(self(), From, DaoMsg#daoMsg.dodagId),
      loop(RootCount, Version, Mop);


% Got From ACK on his DAO message, Distribute the network
    {daoAckMsg, From, DaoAckMsg} ->
      put({?PARENT, DaoAckMsg#daoAckMsg.dodagId}, From), % Update Parent
      {Version, Rank} = get({?VERSION_RANK, DaoAckMsg#daoAckMsg.dodagId}),
      {MyNode, Neighbors} = utils:findMeAndNeighbors(self()),
      io:format("node number: ~p Continue To Build,~n From : ~p~n~n", [RootCount, MyNode]),
      rpl_msg:sendDioToNeighbors(self(), DaoAckMsg#daoAckMsg.dodagId, Rank, Version, Mop, Neighbors),
      % rpl_msg:sendDioToNeighbors(),
      loop(RootCount, Version, Mop);


  % 3 Messages for building the downward Digraph
    {downwardDigraphBuild} ->
      NodeList = ets:tab2list(nodeList),
      put(?DOWNWARD_DIGRAPH, utils:buildVertexDigraph(NodeList)),
      io:format("root number: ~p Starts Building Downward Digraph~n~n", [RootCount]),
      % FROM,DODOAG, NodeList
      utils:requestParent(self(), self(), NodeList),
      loop(RootCount, Version, Mop);

    {requestParent, From, DodagID} ->
      case get({?PARENT, DodagID}) of
        undefined -> % NEED TO UPDATE
          true;
        Parent -> From ! {giveParent, DodagID, self(), Parent}
      end,
      loop(RootCount, Version, Mop);


    {giveParent, DodagID, From, Parent} ->
      io:format("giveParent, DodagID: ~p myNode: ~p Child: ~p Parent: ~p~n", [DodagID, self(), From, Parent]),
      DownwardDigraph = get(?DOWNWARD_DIGRAPH),
      digraph:add_edge(DownwardDigraph, Parent, From),
      put(?DOWNWARD_DIGRAPH, DownwardDigraph),
      loop(RootCount, Version, Mop);

    {sendMessage, {From, To, Msg}} ->

      From;

  %TODO - FOR DEBUG ONLY
    {getAllPath} ->
      NodeList = ets:tab2list(nodeList),
      getAllPath(NodeList)
  end.


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