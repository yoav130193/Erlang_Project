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
  {_,{Pid,Ref}}  = rootServer:start_link({RootCount, Mop}),
  gen_server:call({global,?etsServer},{insert,?nodeControlETS,{node(),Pid}}),
  {reply, {Pid,Ref}, State};

handle_call({spawnNode,NodeCount,Mop}, _From, State) ->
  {_,{Pid,Ref}}  = nodeServer:start_link({NodeCount, Mop}),
  gen_server:call({global,?etsServer},{insert,?nodeControlETS,{node(),Pid}}),
  {reply,{Pid, Ref},State};

handle_call({startRPL,Mop},_From,State = #rplWrapper_state{}) ->
  Reply = rplServer:start_link(Mop),
  {reply,Reply,State};


handle_call(isalive,_From, State) ->
  {reply, alive, State};

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

handle_info({'DOWN', Ref, process, Pid, {Info, Reason, ProcState}}, State= #rplWrapper_state{}) ->
  io:format("rplServer Monitor crash, Ref: ~p , Pid: ~p, Info: ~p, Reason: ~p State: ~p~n", [Ref, Pid, Info, Reason, ProcState]),
  case utils:getCorrectNodeToSpawn(gfx) of
    ?R_NODE -> rpc:call(?R_NODE,rootWrapper,startRPL,[ProcState]);
    ?G_NODE -> rpc:call(?G_NODE,rplWrapper,startRPL,[ProcState]);
    ?N_NODE -> rpc:call(?N_NODE,nodeWrapper,startRPL,[ProcState]);
    ?M_NODE -> rplServer:start_link(ProcState)
  end,
  io:format("RPL crashed, restarted RPL server~n"),
  {noreply, State};
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