%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2020 3:29 PM
%%%-------------------------------------------------------------------
-module('app').
-author("amir").
-include("include/header.hrl").
-behavior(application).
%% API
-export([start/2, stop/1,draw/0]).


start(_StartType,_StartArgs) ->
  register(appProcess, self()),
  io:format("pid for app ~p~n",[whereis(?APP_MAIN_PID)]),
  script:compileAll(),
  gfx_server:start(1),
  appLoop().
  %rplServer:start_link(?NON_STORING).
  %spawn(interruptor,loop,[150]).

stop(State) ->
  erlang:error(not_implemented).

appLoop() ->
  receive
    {'ETS-TRANSFER',TableId,FromPid,HeirData} ->
      io:format("Got  ets transfer ~n"),
      appLoop();
    {startDraw,_FromPid} ->
      spawn(interruptor,loop,[150]),
      appLoop();
    {gfxTerminate,_FromPid,_Reason} ->
      io:format("Got gfx termination for reason ~p~n",[_Reason]),
      if
        is_pid(_Reason) -> exitLoop();
        true ->
          gfx_server:start(1),
          appLoop()
      end;
    Other ->
      io:format("I don't know what the area of a ~p is ~n",[Other]),
      appLoop()
  end.

draw() -> spawn(interruptor,loop,[150]).

exitLoop() -> ok.