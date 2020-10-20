%%%-------------------------------------------------------------------
%%% @author amirsarusi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rplWrapper).
-include("include/header.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,startRPL/1,rpcReq/1]).

-define(SERVERmy, ?MODULE).

-record(rplWrapper_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({global, ?SERVERmy}, ?MODULE, [], []).

init([]) ->
  {ok, #rplWrapper_state{}}.

handle_call({spawnRoot,RootCount,Mop}, _From, State = #rplWrapper_state{}) ->
  {Pid,Ref} = rootServer:start_link({RootCount, Mop}),
  {reply, {ok,{Pid,Ref}}, State};

handle_call({spawnNode,NodeCount,Mop}, _From, State) ->
  {Pid, Ref} = nodeServer:start_link({NodeCount, Mop}),
  {reply,{ok, {Pid, Ref}},State};

handle_call(_Request, _From, State = #rplWrapper_state{}) ->
  {reply, ok, State}.


handle_cast({startRPL,Mop}, State = #rplWrapper_state{}) ->
  rplServer:start_link(Mop),
  {noreply,State};

handle_cast({addNode,root}, State = #rplWrapper_state{}) ->
  rplServer:rpcReq({addNode,root}),
  {noreply,State};

handle_cast(_Request, State = #rplWrapper_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #rplWrapper_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #rplWrapper_state{}) ->
  ok.

code_change(_OldVsn, State = #rplWrapper_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
startRPL(Mop) ->
  io:format("got into startRPL~n"),
  gen_server:cast({global, ?SERVERmy},{startRPL,Mop}).

rpcReq(Request) ->
  io:format("got into startRPL~n"),
  gen_server:cast({global, ?SERVERmy},Request).