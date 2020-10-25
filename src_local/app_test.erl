%%%-------------------------------------------------------------------
%%% @author adar
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2019 12:47 PM
%%%-------------------------------------------------------------------
-module(app_test).
-author("adar").
-include("include/header.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-behaviour(gen_server).

%% API
-export([start/1, start/0, isAlive/0, req_connect_nodes/0, req_start_app/0, startAndConnect/1,makeNodeMap/1,parseXML/0,lta/1,checkAllNodes/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,startRun/0]).

-define(mySERVER, ?MODULE).

-record(appstate, {nodesList, activeNodes, gfx,etsServer, rplserver, name,interruptor}).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start(Name) ->
  gen_server:start_link({global,Name}, ?MODULE, ?MODULE, []).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

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
  {ok, State :: #appstate{}} | {ok, State :: #appstate{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init(Name) ->
  NodeList = parseXML(),
  State = #appstate{activeNodes = [],nodesList = NodeList,gfx = false,rplserver = false,name = Name},
  ets:new(?appEts, [set, named_table, public]),
  io:format("app finished init with pid ~p, nodes from XML ~p~n",[self(),NodeList]),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #appstate{}) ->
  {reply, Reply :: term(), NewState :: #appstate{}} |
  {reply, Reply :: term(), NewState :: #appstate{}, timeout() | hibernate} |
  {noreply, NewState :: #appstate{}} |
  {noreply, NewState :: #appstate{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #appstate{}} |
  {stop, Reason :: term(), NewState :: #appstate{}}).

handle_call(isalive,_From, State) ->
  {reply, alive, State};

handle_call(checkConnections,_From, State=#appstate{nodesList = NodesList}) ->
  checkAllNodes(NodesList),
  {reply, ok, State};

%% connect nodes to the application

handle_call(connectNodes,_From, State=#appstate{nodesList = NodeList}) ->
  makeNodeMap(NodeList),
  NewState = case connectNodes(State,NodeList) of
               {egg,_} -> State#appstate{activeNodes = []};
               {ok,ActiveNodes} ->
                  State#appstate{activeNodes = ActiveNodes}
             end,
  {reply, ok, NewState};

handle_call({spawnRoot,RootCount,Mop},_From, State=#appstate{}) ->
  io:format("got addroot app~n"),
  {_,{Pid,Ref}} = rootServer:start_link({RootCount, Mop}),
  gen_server:call({global,?etsServer},{insert,?nodeControlETS,{node(),Pid}}),
  {reply,{Pid,Ref},State};

handle_call({spawnNode,RootCount,Mop},_From, State=#appstate{}) ->
  io:format("got addroot app~n"),
  {_,{Pid,Ref}} = nodeServer:start_link({RootCount, Mop}),
  gen_server:call({global,?etsServer},{insert,?nodeControlETS,{node(),Pid}}),
  {reply,{Pid,Ref},State};

%% starting the graphics and request empty playground from the first node in queue-global

handle_call(start_app,_From, State = #appstate{nodesList = [_H|_T],activeNodes = [Ah|At]}) ->
  {reply,ok, State#appstate{gfx = true,etsServer = true,activeNodes = At ++ Ah,interruptor = nullptr}};

handle_call({rpl,StoringMode},_From, State = #appstate{}) ->
  Res = rplServer:start_link(StoringMode),
  {reply,Res, State#appstate{gfx = true,etsServer = true}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #appstate{}) ->
  {noreply, NewState :: #appstate{}} |
  {noreply, NewState :: #appstate{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #appstate{}}).

handle_cast(drawGFX,State) ->
  InterruptorPid = spawn(interruptor,loop,[150]),
  io:format("spawned interruptor for gfx_server~n"),
  NewState = State#appstate{interruptor = InterruptorPid},
  {noreply,NewState};


handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #appstate{}) ->
  {noreply, NewState :: #appstate{}} |
  {noreply, NewState :: #appstate{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #appstate{}}).

handle_info({'DOWN', Ref, process, Pid, {gfxCrash, Reason = quit, ProcState}}, State= #appstate{interruptor = Interruptor}) ->
  exit(Interruptor,normal),
  io:format("Gfx exit, Pid: ~p, Info: ~p, Reason: ~p State: ~p~n", [Ref, Pid, Reason, ProcState]),
  gfx_server:start_global(node(),ProcState),
  io:format("restarted Gfx server~n"),
  {noreply, State};
handle_info({'DOWN', Ref, process, Pid, {gfxCrash, Reason, ProcState}}, State= #appstate{interruptor = Interruptor}) ->
  exit(Interruptor,normal),
  io:format("Gfx server crash , Pid: ~p, Info: ~p, Reason: ~p State: ~p~n", [Ref, Pid, Reason, ProcState]),
  gfx_server:start_global(node(),ProcState),
  io:format("restarted Gfx server~n"),
  {noreply, State};

handle_info(_Info, State) ->
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
    State :: #appstate{}) -> term()).
terminate(Reason, State) -> exit({Reason,State}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #appstate{},
    Extra :: term()) ->
  {ok, NewState :: #appstate{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%connect list of nodes
connectNodes(State=#appstate{},[H|T]) ->
  io:format("~p~n",[ets:lookup(?appEts,H)]),
  io:format("~p~n",[H]),
  [{_String,Atom}] = ets:lookup(?appEts,H),
  Res = case net_kernel:connect_node(Atom) of
           true -> true ;
          false -> false;
          _ -> undefined
        end,
  io:format("Res = ~p~n",[Res]),
  connectNodes(State,Res,H,T).

connectNodes(State=#appstate{activeNodes = ActiveNodes},true,H,[]) ->
  io:format("Connected to ~p~n",[H]),
  {ok,State#appstate{activeNodes = ActiveNodes ++ H}};
connectNodes(State=#appstate{},false,H,[]) ->
  io:format("Can't Connect to Node ~p~n",[H]),
  {ok,State};
connectNodes(State=#appstate{},false,H,T) ->
  io:format("Can't Connect to Node ~p~n",[H]),
  [{_String,Atom}] = ets:lookup(?appEts,H),
  connectNodes(State,net_kernel:connect_node(Atom),hd(T),tl(T));
connectNodes(State=#appstate{activeNodes = ActiveNodes},true,H,T) ->
  io:format("Connected to ~p~n",[H]),
  [{_String,Atom}] = ets:lookup(?appEts,hd(T)),
  connectNodes(State#appstate{activeNodes = ActiveNodes ++ H},net_kernel:connect_node(Atom),hd(T),tl(T)).

isAlive() -> gen_server:call({global,node()},isalive).

%% connects given list of nodes names

req_connect_nodes() ->
  net_kernel:connect_node('m_node@amirs-MacBook-Pro'),
  net_kernel:connect_node('g_node@amirs-MacBook-Pro'),
  net_kernel:connect_node('n_node@amirs-MacBook-Pro'),
  net_kernel:connect_node('r_node@amirs-MacBook-Pro').


  %gen_server:call({global,node()},connectNodes).

%%opens control and graphics in the requested node

req_start_app() ->
  etsServer:start_link(),
  gfx_server:start_global(node()).
  %gen_server:call({global,node()},start_app).

startAndConnect(Name) -> start(Name).

parseXML() ->
  {Xml,_} = xmerl_scan:file("erlangConfig.xml",[{validation,true}]),
  Content = Xml#xmlElement.content,
  MetaRec = [Data || {_,_,_,_,_,_,_,_,Data,_,_,_} <- Content],
  [NodeName || [{_,_,_,_,NodeName,_}] <- MetaRec].

makeNodeMap([H|[]]) -> ets:insert(?appEts,{H,list_to_atom(H)});
makeNodeMap(ActiveNodes) ->
  io:format("~p~n",[hd(ActiveNodes)]),
  ets:insert(?appEts,{hd(ActiveNodes),list_to_atom(hd(ActiveNodes))}),
  makeNodeMap(tl(ActiveNodes)).

lta(List) -> list_to_atom(List).

checkAllNodes(_NodesList) ->

  GnodeStats = global:whereis_name(rplWrapper),
  RnodeStats = global:whereis_name(rootWrapper),
  NnodeStats = global:whereis_name(nodeWrapper),
  case GnodeStats of
    undefined  ->
      gen_server:call({global,?etsServer},{deleteNodePid,?G_NODE});
    _ -> ok
  end,
  case RnodeStats of
    undefined  ->
      gen_server:call({global,?etsServer},{deleteNodePid,?R_NODE});
    _ -> ok
  end,
  case NnodeStats of
    undefined  ->
      gen_server:call({global,?etsServer},{deleteNodePid,?N_NODE});
    _ -> ok
  end.

startRun() ->
  script:compileAll(),
  startAndConnect(node()),
  req_connect_nodes(),
  req_start_app().




