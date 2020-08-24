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

-behavior(application).
%% API
-export([start/2, stop/1]).


start(StartType, StartArgs) ->

  erlang:error(not_implemented).

stop(State) ->
  erlang:error(not_implemented).