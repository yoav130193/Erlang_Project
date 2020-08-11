%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2020 15:31
%%%-------------------------------------------------------------------
-module(script).
-author("yoavlevy").

%% API
-export([startAll/0, addNode/2, checkLists/0, script1/0, addMultipleNodes/3, randommmmm/0, compileAll/0, startAllReal/0]).
-import(gfx_server, [start/1]).
-define(SERVER, rplServer).

script1() ->
  {_, ServerPid} = startAll(),
  io:format("My Server Pid: ~p~n", [ServerPid]),
  addMultipleNodes(ServerPid, root, 2),
  addMultipleNodes(ServerPid, normal, 5),
  checkLists().

startAll() ->
  io:format("compile all files and start server~n"),
  compileAll(),
  rplServer:start_link(1).
%gen_server:start_link({local, ?SERVER}, ?SERVER, [1], []).

startAllReal() ->
  io:format("compile all files and start server~n"),
  compileAll(),
  gen_server:start_link({local, ?SERVER}, ?SERVER, [1], []),
  gfx_server:start(1).

compileAll() ->
  compile:file(rplServer),
  compile:file(nodeServer),
  compile:file(rootServer),
  %compile:file(rootLoop),
  %compile:file(nodeLoop),
  compile:file(rpl_msg),
  compile:file(gfx_server),
  compile:file(funcGenerator),
  compile:file(utils).

addNode(Server_Pid, root) ->
  gen_server:call(Server_Pid, {addNode, root});

addNode(Server_Pid, normal) ->
  gen_server:call(Server_Pid, {addNode, normal}).


addMultipleNodes(Server_Pid, Which, 0) -> finish;
addMultipleNodes(Server_Pid, Which, N) ->
  addNode(Server_Pid, Which),
  addMultipleNodes(Server_Pid, Which, N - 1).

checkLists() ->
  io:format("root List: ~p~n", [ets:tab2list(rootList)]),
  io:format("node List: ~p~n", [ets:tab2list(nodeList)]).


randommmmm() ->
  random:seed(1),
  [random:uniform(1000) || _ <- lists:seq(1, 50)].

