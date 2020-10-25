%%%-------------------------------------------------------------------
%%% @author amirsarusi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(nodeWrapper).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3,startRPL/1]).
-include("include/header.hrl").
-define(mySERVER, ?MODULE).

-record(nodeWrapper_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({global, ?mySERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #nodeWrapper_state{}}.

handle_call({spawnNode,NodeCount,Mop}, _From, State) ->
  {_,{Pid,Ref}} = nodeServer:start_link({NodeCount, Mop}),
  gen_server:call({global,?etsServer},{insert,?nodeControlETS,{node(),Pid}}),
  {reply,{Pid, Ref},State};

handle_call(isalive,_From, State) ->
  {reply, alive, State};

handle_call({startRPL,Mop},_From,State = #nodeWrapper_state{}) ->
  Reply = rplServer:start_link(Mop),
  {reply,Reply,State};

handle_call({spawnRoot,RootCount,Mop}, _From, State = #nodeWrapper_state{}) ->
  {_,{Pid,Ref}} = rootServer:start_link({RootCount, Mop}),
  gen_server:call({global,?etsServer},{insert,?nodeControlETS,{node(),Pid}}),
  {reply,{Pid,Ref}, State};

handle_call(_Request, _From, State = #nodeWrapper_state{}) ->
  {reply, ok, State}.

handle_cast({startRPL,Mop}, State = #nodeWrapper_state{}) ->
  global:unregister_name(rplServer),
  rplServer:start_link(Mop),
  {noreply, State};

handle_cast(_Request, State = #nodeWrapper_state{}) ->
  {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, {Info, Reason, ProcState}}, State= #nodeWrapper_state{}) ->
  io:format("rplServer Monitor crash, Ref: ~p , Pid: ~p, Info: ~p, Reason: ~p State: ~p~n", [Ref, Pid, Info, Reason, ProcState]),
  case utils:getCorrectNodeToSpawn(gfx) of
    ?R_NODE -> rpc:call('r_node@amirs-MacBook-Pro',rootWrapper,startRPL,[ProcState]);
    ?G_NODE -> rpc:call('g_node@amirs-MacBook-Pro',rplWrapper,startRPL,[ProcState]);
    ?N_NODE -> rpc:call('n_node@amirs-MacBook-Pro',nodeWrapper,startRPL,[ProcState]);
    ?M_NODE -> rplServer:start_link(ProcState)
  end,
  io:format("RPL crashed, restarted RPL server~n"),
  {noreply, State};
handle_info(_Info, State = #nodeWrapper_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #nodeWrapper_state{}) ->
  ok.

code_change(_OldVsn, State = #nodeWrapper_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
startRPL(Mop) ->
  io:format("got into startRPL~n"),
  gen_server:cast({global, ?mySERVER},{startRPL,Mop}).