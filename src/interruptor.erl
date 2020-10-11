%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Aug 2020 5:28 PM
%%%-------------------------------------------------------------------
-module(interruptor).
-author("amir").
%% API
-export([loop/1]).

loop(Timeout) ->
  wx_object:cast(gfx_server,draw),
  %io:format("interrupting ~n"),
  timer:sleep(Timeout),
  loop(Timeout).