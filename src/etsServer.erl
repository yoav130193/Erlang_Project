%%%-------------------------------------------------------------------
%%% @author amirsarusi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2020 10:26
%%%-------------------------------------------------------------------
-module(etsServer).
-author("amirsarusi").
-behaviour(gen_server).
-include("include/header.hrl").
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).
-include_lib("wx/include/wx.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%%-define(SERVER, ?MODULE).
-record(etsState, {started}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("ets server start link~n"),
  gen_server:start_link({global, ?MODULE}, ?MODULE, [local,node()], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #etsState{}} | {ok, State :: #etsState{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([_Mode,_Node]) ->
  io:format("start ets init~n"),
  ets:new(?locationEts, [set, named_table, public,{read_concurrency, true}]),
  ets:new(?nodePidsEts, [bag, named_table, public]),
  ets:new(?pidStringEts,[set, named_table, public]),
  ets:new(?ROOT_LIST, [set, named_table, public]),
  ets:new(?MSG_TABLE, [set, named_table, public]),
  ets:new(?RPL_REF, [set, named_table, public]),
  ets:new(?DOWNWARD_DIGRAPH, [set, named_table, public]),
  io:format("Finished ETS server init ~n"),
  NewState = #etsState{started = true},
  %gfx_server:start_global('g_node@amirs-Macbook-Pro'),
  %rpc:call('g_node@amirs-Macbook-Pro',gfx_server,gfx_server:start_global,['g_node@amirs-Macbook-Pro']),
  {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #etsState{}) ->
  {reply, Reply :: term(), NewState :: #etsState{}} |
  {reply, Reply :: term(), NewState :: #etsState{}, timeout() | hibernate} |
  {noreply, NewState :: #etsState{}} |
  {noreply, NewState :: #etsState{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #etsState{}} |
  {stop, Reason :: term(), NewState :: #etsState{}}).

handle_call({lookup,TableName,Key}, _From, State = #etsState{}) ->
  Reply = ets:lookup(TableName,Key),
  {reply, Reply, State};

handle_call({whereis,TableName}, _From, State = #etsState{}) ->
  Reply = ets:whereis(TableName),
  {reply, Reply, State};

handle_call({getAll,TableName}, _From, State = #etsState{}) ->
  Reply = ets:tab2list(TableName),
  {reply, Reply, State};

handle_call({delete,TableName,Key},_From,State = #etsState{}) ->
  ets:delete(TableName,Key),
  {reply, ok, State};

handle_call({deleteAll,TableName},_From,State = #etsState{}) ->
  ets:delete_all_objects(TableName),
  {reply, ok, State};

handle_call({insert,TableName,{Key,Value}},_From,State = #etsState{}) ->
  ets:insert(TableName,{Key,Value}),
  {reply, ok, State};

handle_call(_Request, _From, State = #etsState{}) ->
{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #etsState{}) ->
  {noreply, NewState :: #etsState{}} |
  {noreply, NewState :: #etsState{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #etsState{}}).

handle_cast({delete,TableName,Key}, State = #etsState{}) ->
  ets:delete_object(TableName,Key),
  {noreply, State};

handle_cast({deleteAll,TableName}, State = #etsState{}) ->
  ets:delete_all_objects(TableName),
  {noreply, State};

handle_cast({insert,TableName,{Key,Value}}, State = #etsState{}) ->
  ets:insert(TableName,{Key,Value}),
  {noreply, State};

handle_cast(_Request, State = #etsState{}) ->
{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #etsState{}) ->
  {noreply, NewState :: #etsState{}} |
  {noreply, NewState :: #etsState{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #etsState{}}).

handle_info(_Info, State = #etsState{}) ->
{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #etsState{}) -> term()).

terminate(Reason, _State) ->
  gen_server:cast({global,?APP_SERVER},{etsServerCrash,Reason}),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #etsState{},
    Extra :: term()) ->
  {ok, NewState :: #etsState{}} | {error, Reason :: term()}).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.






