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

-behavior(wx_object).
%% API
-export([init/1, handle_event/2]).


init(Args) ->
  erlang:error(not_implemented).

handle_event(Request, State) ->
  erlang:error(not_implemented).