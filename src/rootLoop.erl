%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2020 11:56
%%%-------------------------------------------------------------------
-module(rootLoop).
-author("yoavlevy").

%% API
-export([loop/1, loopStart/1]).

loopStart(RootCount) ->
  io:format("new root number: ~p~n ", [RootCount]),
  loop(RootCount).

% Locations = list of {NodePid,X,Y}
loop(RootCount) ->
  receive
    {locations, Locations} ->
      io:format("root number: ~p got locations: ~p ~n ", [RootCount, Locations]),
      {MyNode, OtherNodeList} = findMySelf(Locations),
      Neighbors = utils:findNeighbors(MyNode, OtherNodeList),
      loop(RootCount)
  end.


findMySelf(Location) ->
  ets:lookup()
  Locations.