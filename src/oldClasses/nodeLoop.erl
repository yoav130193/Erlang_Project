%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2020 11:56
%%%-------------------------------------------------------------------
-module(nodeLoop).
-author("yoavlevy").

%% API
-export([loopStart/1]).


-record(dioMsg, {rplInstanceId, versionNumber, rank, g = 2#1, zero = 2#0, mop, prf = 2#000, dtsn, flags = 16#00, reserved = 16#00, dodagId}).
-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId}).
-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId}).

-define(VERSION_RANK, version_rank).
-define(PARENT, parent).

loopStart(NodeCount) ->
  io:format("new node :)~n"),
  Mop = element(2, hd(ets:lookup(mop, mopKey))),
  loop(NodeCount, Mop).

loop(NodeCount, Mop) ->

  receive
  % Got DIO msg -> send a DAO message to join the DODAG
    {dioMsg, From, DioMsg} ->
      UpdateNeeded = utils:checkIfUpdateNeeded(DioMsg#dioMsg.dodagId, DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1, From),
      if
        UpdateNeeded =:= true -> % Update rank and version -> send back DAO message
          put({?VERSION_RANK, DioMsg#dioMsg.dodagId}, {DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1}),
          rpl_msg:sendDaoAfterDio(self(), From, DioMsg#dioMsg.dodagId);
        true -> rpl_msg:noNeedToUpdateToFile(self(), From, DioMsg#dioMsg.dodagId)
      end,
      loop(NodeCount, Mop);

%TODO - HALF implemented! Think if need to send to the root message
  %Got DAO message from a node that got DIO -> root/other needs to send dao-ack back
    {daoMsg, From, DaoMsg} ->
      %utils:updateUpwardDigraph(self(), From, DaoMsg),
      rpl_msg:sendDaoAckAfterDao(self(), From, DaoMsg#daoMsg.dodagId),
      loop(NodeCount, Mop);

  % Got From ack on his DAO message, Distribute the network
    {daoAckMsg, From, DaoAckMsg} ->
      put({?PARENT, DaoAckMsg#daoAckMsg.dodagId}, From), % Update Parent
      {Version, Rank} = get({?VERSION_RANK, DaoAckMsg#daoAckMsg.dodagId}),
      {MyNode, Neighbors} = utils:findMeAndNeighbors(self()),
      io:format("node number: ~p Continue To Build,~n From : ~p~n~n", [NodeCount, MyNode]),
      rpl_msg:sendDioToNeighbors(self(), DaoAckMsg#daoAckMsg.dodagId, Rank, Version, Mop, Neighbors),
      % rpl_msg:sendDioToNeighbors(),
      loop(NodeCount, Mop);

    % Got a request for parents
    {requestParent, From, DodagID} ->
      case get({?PARENT, DodagID}) of
        undefined -> % NEED TO UPDATE
          io:format("requestParent, UNDEFIEND! DodagID: ~p node: ~p got from :~p~n", [DodagID, self(), From]);
        Parent ->
          io:format("requestParent, FIND! DodagID: ~pnode: ~p got from :~p Parent: ~p~n", [DodagID, self(), From, Parent]),
          From ! {giveParent, DodagID, self(), Parent}
      end,
      loop(NodeCount, Mop)

  end.

