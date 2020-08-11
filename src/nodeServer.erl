%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2020 12:18
%%%-------------------------------------------------------------------
-module(nodeServer).
-author("yoavlevy").
-behavior(gen_server).
%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/1, terminate/2]).

-record(dioMsg, {rplInstanceId, versionNumber, rank, g = 2#1, zero = 2#0, mop, prf = 2#000, dtsn, flags = 16#00, reserved = 16#00, dodagId}).
-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId}).
-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId}).

-define(VERSION_RANK, version_rank).
-define(PARENT, parent).
-define(NODE_SERVER, nodeServer).

start_link(NodeCount) ->
  gen_server:start_link({local, list_to_atom("node_server" ++ integer_to_list(NodeCount))}, ?NODE_SERVER, [NodeCount], []).

init(NodeCount) ->
  io:format("new node :)~n"),
  Mop = element(2, hd(ets:lookup(mop, mopKey))),
  {ok, {NodeCount, Mop}}.

handle_cast({dioMsg, From, DioMsg}, State) ->
  UpdateNeeded = utils:checkIfUpdateNeeded(DioMsg#dioMsg.dodagId, DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1, From),
  if
    UpdateNeeded =:= true -> % Update rank and version -> send back DAO message
      put({?VERSION_RANK, DioMsg#dioMsg.dodagId}, {DioMsg#dioMsg.versionNumber, DioMsg#dioMsg.rank + 1}),
      rpl_msg:sendDaoAfterDio(self(), From, DioMsg#dioMsg.dodagId, State);
    true -> rpl_msg:noNeedToUpdateToFile(self(), From, DioMsg#dioMsg.dodagId)
  end,
  {noreply, State};

% Got From ack on his DAO message, Distribute the network
handle_cast({daoAckMsg, From, DaoAckMsg}, {NodeCount, Mop}) ->
  put({?PARENT, DaoAckMsg#daoAckMsg.dodagId}, From), % Update Parent
  {Version, Rank} = get({?VERSION_RANK, DaoAckMsg#daoAckMsg.dodagId}),
  {MyNode, Neighbors} = utils:findMeAndNeighbors(self()),
  io:format("node number: ~p Continue To Build,~n From : ~p~n~n", [NodeCount, MyNode]),
  rpl_msg:sendDioToNeighbors(self(), DaoAckMsg#daoAckMsg.dodagId, Rank, Version, Mop, Neighbors),
% rpl_msg:sendDioToNeighbors(),
  {noreply, {NodeCount, Mop}};

% Got a request for parents
handle_cast({requestParent, From, DodagID}, {NodeCount, Mop}) ->
  case get({?PARENT, DodagID}) of
    undefined -> % NEED TO UPDATE
      io:format("requestParent, UNDEFIEND, DodagID: ~p node: ~p got from :~p~n", [DodagID, self(), From]);
    Parent ->
      io:format("requestParent, FIND, DodagID: ~pnode: ~p got from :~p Parent: ~p~n", [DodagID, self(), From, Parent]),
      gen_server:cast(From, {giveParent, DodagID, self(), Parent})
    %   From ! {giveParent, DodagID, self(), Parent}
  end,
  {noreply, {NodeCount, Mop}};


%TODO - HALF implemented, Think if need to send to the root message
%Got DAO message from a node that got DIO -> root/other needs to send dao-ack back
handle_cast({daoMsg, From, DaoMsg}, State) ->
  rpl_msg:sendDaoAckAfterDao(self(), From, DaoMsg#daoMsg.dodagId, State),
  {noreply, State}.



terminate(_Reason, _State) ->
  io:format("nodeServer terminate~n"),
  [].


handle_call(Request, From, State) ->
  erlang:error(not_implemented).