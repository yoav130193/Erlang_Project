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
-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId, updateType}).
-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId, updateType}).

-define(MY_DODAGs, my_Dodags).
-define(VERSION_RANK, version_rank).
-define(PARENT, parent).
-define(NODE_SERVER, nodeServer).
-define(MSG_TABLE, msgTable).
-define(RPL_REF, rplRef).


-record(msg_table_key, {dodagId, from, to}).

start_link({NodeCount, Mop}) ->
  gen_server:start_link({local, list_to_atom("node_server" ++ integer_to_list(NodeCount))}, ?NODE_SERVER, [{NodeCount, Mop}], []).

init([{NodeCount, Mop}]) ->
  io:format("new node :)~n"),
  % Mop = element(2, hd(ets:lookup(mop, mopKey))),
  {ok, {NodeCount, Mop}}.

%****************     RPL PROTOCOL MESSAGES     *****************%

handle_cast({dioMsg, From, DioMsg}, State) ->
  rpl_msg:handleDioMsg(DioMsg, From, State),
  {noreply, State};


%TODO - HALF implemented, Think if need to send to the root message
%Got DAO message from a node that got DIO -> root/other needs to send dao-ack back
handle_cast({daoMsg, From, DaoMsg}, State) ->
  rpl_msg:sendDaoAckAfterDao(self(), From, DaoMsg#daoMsg.dodagId, DaoMsg#daoMsg.updateType, State),
  {noreply, State};


% Got From ack on his DAO message, Distribute the network
handle_cast({daoAckMsg, From, DaoAckMsg}, {NodeCount, Mop}) ->
  rpl_msg:handleDaoAck({From, DaoAckMsg}, {NodeCount, [], Mop}),
  {noreply, {NodeCount, Mop}};

%*****************    DIGRAPH BUILD     *****************%

% Got a request for parents
handle_cast({requestParent, From, DodagID}, {NodeCount, Mop}) ->
  case get({?PARENT, DodagID}) of
    undefined -> % NEED TO UPDATE
      utils:deleteMessageFromEts(DodagID, From, self(), {finishedDigraphBuilding}, requestParentNode);
    Parent ->
      gen_server:cast(From, {giveParent, DodagID, self(), Parent})
  end,
  {noreply, {NodeCount, Mop}};

%*****************    SENDING A MESSAGE     *****************%

handle_cast({parentMsg, From, To, DodagID, Msg, PathToRoot}, State) ->
  io:format("parentMsg, DodagID: ~p myNode: ~p msg: ~p, From: ~p, To: ~p~n", [DodagID, self(), Msg, From, To]),
  gen_server:cast(hd(get({?PARENT, DodagID})), {parentMsg, From, To, DodagID, Msg, PathToRoot ++ [self()]}),
  {noreply, State};

handle_cast({downwardMessage, From, To, Msg, DodagID, PathList, WholePath}, State) ->
  utils:handleDownwardMessage(DodagID, Msg, From, To, WholePath, PathList),
  {noreply, State}.


terminate(_Reason, _State) ->
  io:format("nodeServer terminate~n"),
  [].

handle_call({sendMessageStoring, From, To, Msg}, OrderFrom, {NodeCount, Mop}) ->
  ets:insert(?RPL_REF, {ref, OrderFrom}),
  io:format("OrderFrom: ~p~n", [OrderFrom]),
  utils:sendMessageStoring(From, To, Msg, Mop),
  {noreply, {NodeCount, Mop}};


handle_call({sendMessageNonStoring, From, To, Msg}, OrderFrom, {NodeCount, Mop}) ->
  ets:insert(?RPL_REF, {ref, OrderFrom}),
  io:format("OrderFrom: ~p~n", [OrderFrom]),
  utils:sendMessageNonStoring(From, To, Msg, Mop),
  {noreply, {NodeCount, Mop}};

handle_call(Request, From, State) ->
  erlang:error(not_implemented).