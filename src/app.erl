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
  script:compileAll(),
  gfx_server:start(1),
  rplServer:start_link(?NON_STORING).
  %spawn(interruptor,loop,[150]).

stop(State) ->
  erlang:error(not_implemented).



draw() -> spawn(interruptor,loop,[150]).