%%%-------------------------------------------------------------------
%%% @author amirsarusi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rootWrapper).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-include("include/header.hrl").
-define(mySERVER, ?MODULE).

-record(rootWrapper_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({global, ?mySERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #rootWrapper_state{}}.

handle_call({spawnRoot,RootCount,Mop}, _From, State = #rootWrapper_state{}) ->
  {Pid,Ref} = rootServer:start_link({RootCount, Mop}),
  {reply, {ok,{Pid,Ref}}, State};

handle_call({spawnNode,NodeCount,Mop}, _From, State) ->
  {Pid, Ref} = nodeServer:start_link({NodeCount, Mop}),
  {reply,{ok, {Pid, Ref}},State};

handle_call(_Request, _From, State = #rootWrapper_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #rootWrapper_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #rootWrapper_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #rootWrapper_state{}) ->
  ok.

code_change(_OldVsn, State = #rootWrapper_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

