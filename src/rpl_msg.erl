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
-include("include/header.hrl").


%% API
-export([sendDioToNeighbors/6, sendDaoAfterDio/5, sendDaoAckAfterDao/5, handleDaoAck/2, handleDioMsg/3, getParents/1]).


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
    UpdateNeeded =:= ?Update -> % Update rank and version -> send back DAO message
      put({?VERSION_RANK, DioMsg#dioMsg.dodagId}, {DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1}),
      rpl_msg:sendDaoAfterDio(self(), From, DioMsg#dioMsg.dodagId, ?Update, State);
    UpdateNeeded =:= ?Update_Addition ->
      rpl_msg:sendDaoAfterDio(self(), From, DioMsg#dioMsg.dodagId, ?Update_Addition, State);
  %utils:deleteMessageFromEts(DioMsg#dioMsg.dodagId, From, self(), {finishedBuilding}, dio);
    true ->
      utils:deleteMessageFromEts(DioMsg#dioMsg.dodagId, From, self(), {finishedBuilding}, dio)
  end.

% ***********   DAO MSG   ***********%

%DAO Destination Advertisment Object)
% unicast message from all the node to the root, this message is a request to join the DODAG
daoMsg(DodagId, UpdateType) ->
  #daoMsg{rplInstanceId = 0, daoSequence = 0, dodagId = DodagId, updateType = UpdateType}.

sendDaoAfterDio(From, To, DodagId, UpdateType, State) ->
  DaoMsg = daoMsg(DodagId, UpdateType),
  %saveDaoToFile(From, To, DodagId),
  gen_server:cast(To, {daoMsg, From, DaoMsg}).


% ***********   DAO ACK MSG   ***********%

% DAO acknowledge
% After receiving a DAO message, the root sends an ack message that confirms that the node accepted in to the DODAG
daoAckMsg(Dodag, UpdateType) ->
  #daoAckMsg{rplInstanceId = 0, daoSequence = 0, dodagId = Dodag, updateType = UpdateType}.

sendDaoAckAfterDao(From, To, DodagId, UpdateType, State) ->
  DaoAckMsg = daoAckMsg(DodagId, UpdateType),
%  io:format("DODAG_ID: ~p,DAO-ACK message from: ~p to: ~p~nmsg:~p~n~n", [DodagId, From, To, DaoAckMsg]),
  saveDaoAckToFile(From, To, DodagId),
  gen_server:cast(To, {daoAckMsg, From, DaoAckMsg}).


handleDaoAck({From, DaoAckMsg}, {RootCount, Version, Mop}) ->
  addToDodagList(DaoAckMsg#daoAckMsg.dodagId),
  case DaoAckMsg#daoAckMsg.updateType of
    ?Update ->
      put({?PARENT, DaoAckMsg#daoAckMsg.dodagId}, [From]), % Update Parent
      {NewVersion, Rank} = get({?VERSION_RANK, DaoAckMsg#daoAckMsg.dodagId}),
      {_, Neighbors} = utils:findMeAndNeighbors(self()),
      rpl_msg:sendDioToNeighbors(self(), DaoAckMsg#daoAckMsg.dodagId, Rank, NewVersion, Mop, Neighbors);
    ?Update_Addition ->
      updateParent(DaoAckMsg#daoAckMsg.dodagId, From)
  end,
  io:format("Me: ~p , My PARENT LIST: ~p~n", [self(), getParents(DaoAckMsg#daoAckMsg.dodagId)]),
  utils:deleteMessageFromEts(DaoAckMsg#daoAckMsg.dodagId, From, self(), {finishedBuilding}, daoAck).


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

getParents(DodagId) ->
  case get({?PARENT, DodagId}) of
    undefined -> [];
    ParentList -> ParentList
  end.

updateParent(DodagId, NewParent) ->
  case lists:member(NewParent, getParents(DodagId)) of
    false ->
      io:format("NEW Parent DODAG_ID: ~p, Me: ~p , New PARENT: ~p~n", [DodagId, self(), NewParent]),
      put({?PARENT, DodagId}, getParents(DodagId) ++ [NewParent]);% Update Parent
    true -> continue
  end.
