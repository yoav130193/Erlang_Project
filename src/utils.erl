%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2020 19:06
%%%-------------------------------------------------------------------
-module(utils).
-author("yoavlevy").

%% API
-export([findNeighbors/2]).

-define(dis,100).

% Find Your Neighbors
findNeighbors(MyNodeLocation, NodeLocationList) ->
  [Neighbors || Neighbors <- NodeLocationList, calculateDistance(MyNodeLocation, Neighbors) < ?dis].

% Location = {Pid,X,Y}
calculateDistance({_, X_Node_1, Y_Node_1}, {_, X_Node_2, Y_Node_2}) ->
  X_distance = erlang:abs(X_Node_1 - X_Node_2),
  Y_distance = erlang:abs(Y_Node_1 - Y_Node_2),
  math:sqrt(X_distance * X_distance + Y_distance * Y_distance).
