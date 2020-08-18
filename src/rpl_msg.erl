%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2020 14:04
%%%-------------------------------------------------------------------
-module(rpl_msg).
-author("yoavlevy").

%% API
-export([sendDioToNeighbors/6, sendDaoAfterDio/4, sendDaoAckAfterDao/4, handleDaoAck/2, handleDioMsg/3]).

-define(LOG_FILE_NAME, "my_log_file.txt").
-define(VERSION_RANK, version_rank).
-define(PARENT, parent).
-define(MY_DODAGs, my_Dodags).
-define(MSG_TABLE, msgTable).

-record(msg_table_key, {dodagId, from, to}).

% ***********   DIO MSG   ***********%

-record(dioMsg, {rplInstanceId, versionNumber, rank, g = 2#1, zero = 2#0, mop, prf = 2#000, dtsn, flags = 16#00, reserved = 16#00, dodagId}).

% DIO (DODAF Indormation Object)
% This is a multicast message from the root that notifies all the nodes about his DODAG
% TODO - check if this message is only from the root and when exactly this happens
dioMsg(DodagId, Rank, Version, Mop) ->
  %TODO - implement all of the following:
  #dioMsg{rplInstanceId = 0, versionNumber = Version, rank = Rank, mop = Mop, dtsn = 0, dodagId = DodagId}.

% Called from the Nodes to send Dio Msg
sendDioToNeighbors(Pid, DodagId, Rank, Version, Mop, Neighbors) ->
  DioMsg = dioMsg(DodagId, Rank, Version, Mop),
  %io:format("DODAG_ID: ~p ,DIO message from: ~p, sends to: ~p~nmsg: ~p~n~n", [DodagId, Pid, Neighbors, DioMsg]),
  %saveDioToFile(self(), Neighbors, DodagId, Version, Rank),
  lists:foreach(fun(Element) ->
    ets:insert(?MSG_TABLE, {#msg_table_key{dodagId = DioMsg#dioMsg.dodagId, from = self(), to = element(1, Element)}, {msg}}),
    gen_server:cast(element(1, Element), {dioMsg, self(), DioMsg})
%element(1, Element) ! {dioMsg, self(), DioMsg}
                end, Neighbors).

handleDioMsg(DioMsg, From, State) ->
  UpdateNeeded = utils:checkIfUpdateNeeded(DioMsg#dioMsg.dodagId, DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1, From),
  if
    UpdateNeeded =:= true -> % Update rank and version -> send back DAO message
      put({?VERSION_RANK, DioMsg#dioMsg.dodagId}, {DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1}),
      rpl_msg:sendDaoAfterDio(self(), From, DioMsg#dioMsg.dodagId, State);
    true ->
      utils:deleteMessageFromEts(DioMsg#dioMsg.dodagId, From, self(), {finishedBuilding})
  % rpl_msg:noNeedToUpdateToFile(self(), From, DioMsg#dioMsg.dodagId)
  end.

% ***********   DAO MSG   ***********%

-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId}).

%DAO Destination Advertisment Object)
% unicast message from all the node to the root, this message is a request to join the DODAG
% TODO - check if the destination is only the root and if it always happens after DIO
daoMsg(Dodag) ->
  %TODO - implement all of the following:
  #daoMsg{rplInstanceId = 0, daoSequence = 0, dodagId = Dodag}.

sendDaoAfterDio(From, To, DodagId, State) ->
  DaoMsg = daoMsg(DodagId),
  %io:format("DODAG_ID: ~p, DAO message from: ~p to: ~p~nmsg:~p~n~n", [DodagId, From, To, DaoMsg]),
  %saveDaoToFile(From, To, DodagId),
  gen_server:cast(To, {daoMsg, From, DaoMsg}).


% ***********   DAO ACK MSG   ***********%

-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId}).

% DAO acknowledge
% After receiving a DAO message, the root sends an ack message that confirms that the node accepted in to the DODAG
% TODO - Understand if besides root , other nodes sends this message. If yes, the response is different
daoAckMsg(Dodag) ->
  #daoAckMsg{rplInstanceId = 0, daoSequence = 0, dodagId = Dodag}.

sendDaoAckAfterDao(From, To, DodagId, State) ->
  DaoAckMsg = daoAckMsg(DodagId),
%  io:format("DODAG_ID: ~p,DAO-ACK message from: ~p to: ~p~nmsg:~p~n~n", [DodagId, From, To, DaoAckMsg]),
  saveDaoAckToFile(From, To, DodagId),
  gen_server:cast(To, {daoAckMsg, From, DaoAckMsg}).


handleDaoAck({From, DaoAckMsg}, {RootCount, Version, Mop}) ->
  % DodagID = DaoAckMsg#daoAckMsg.dodagId,
  addToDodagList(DaoAckMsg#daoAckMsg.dodagId),
  put({?PARENT, DaoAckMsg#daoAckMsg.dodagId}, From), % Update Parent
  {NewVersion, Rank} = get({?VERSION_RANK, DaoAckMsg#daoAckMsg.dodagId}),
  {MyNode, Neighbors} = utils:findMeAndNeighbors(self()),
  % io:format("DODAG_ID: ~p , node number: ~p Continue To Build,~n From : ~p~n~n", [DaoAckMsg#daoAckMsg.dodagId, RootCount, MyNode]),
  rpl_msg:sendDioToNeighbors(self(), DaoAckMsg#daoAckMsg.dodagId, Rank, NewVersion, Mop, Neighbors),
  utils:deleteMessageFromEts(DaoAckMsg#daoAckMsg.dodagId, From, self(), {finishedBuilding}).


%************   Save info TO Files    ************%

saveDioToFile(Me, Neighbors, DodagId, Version, Rank) ->
  {ok, S} = file:open(?LOG_FILE_NAME, [append]),
  lists:foreach(fun(Element) ->
    io:format(S, "{~p,DIO,~p,~p}, {~p,~p}~n", [DodagId, Me, element(1, Element), Version, Rank]) end, Neighbors).

saveDaoToFile(Me, To, DodagId) ->
  {ok, S} = file:open(?LOG_FILE_NAME, [append]),
  io:format(S, "{~p,DAO,~p,~p}~n", [DodagId, Me, To]).

saveDaoAckToFile(Me, To, DodagId) ->
  {ok, S} = file:open(?LOG_FILE_NAME, [append]),
  io:format(S, "{~p,DAO-ACK,~p,~p}~n", [DodagId, Me, To]).

noNeedToUpdateToFile(Me, To, DodagId) ->
  {ok, S} = file:open(?LOG_FILE_NAME, [append]),
  io:format(S, "{~p,NO-DAO,~p,~p}~n", [DodagId, Me, To]).


addToDodagList(DodagId) ->
  case get(?MY_DODAGs) of
    undefined -> put(?MY_DODAGs, utils:getDodagList() ++ [DodagId]);
    DodagList -> case lists:member(DodagId, DodagList) of
                   true -> continue;
                   false -> put(?MY_DODAGs, utils:getDodagList() ++ [DodagId])
                 end
  end.





