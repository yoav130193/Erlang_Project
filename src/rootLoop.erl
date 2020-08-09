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
      loop(RootCount);
    {buildNetwork} ->
      MyNode = hd(ets:lookup(rootList, self())),
      Y = ets:tab2list(nodeList),
      OtherNodeList = [X || X <- Y, element(1, X) =/= element(1, MyNode)],
      Neighbors = utils:findNeighbors(MyNode, OtherNodeList),

      io:format("root number: ~p start building network, finded in ets: ~p , my neighbors: ~p ~n ", [RootCount, MyNode, Neighbors]),

      loop(RootCount)
  end.


findMySelf(Location) ->
  % ets:lookup()
  Location.