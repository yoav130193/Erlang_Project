%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2020 11:56
%%%-------------------------------------------------------------------
-module(nodeLoop).
-author("yoavlevy").

%% API
-export([loop/0, loopStart/0]).

loopStart() ->
  io:format("new node :)~n"),
  loop().

loop() ->
  receive
    {locations, {NodeList, X_List, Y_List}} ->
      io:format("node: got locations~n ", [])
  end.