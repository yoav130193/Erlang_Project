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
-export([startAll/0, addNode/2, checkLists/0, script1/0, addMultipleNodes/3]).

script1() ->
  {_, ServerPid} = startAll(),
  io:format("My Server Pid: ~p~n",[ServerPid]),
  addMultipleNodes(ServerPid, root, 2),
  addMultipleNodes(ServerPid, normal, 5),
  checkLists().


startAll() ->
  io:format("compile all files and start server~n"),
  compile:file(myServer),
  compile:file(rootLoop),
  compile:file(nodeLoop),
  gen_server:start_link(myServer, [], []).


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

