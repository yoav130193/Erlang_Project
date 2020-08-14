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
-export([findMeAndNeighbors/1, findNeighbors/2, checkIfUpdateNeeded/4, requestParent/3, buildVertexDigraph/1, sendMessage/3, getDodagList/0, calculatePath/1,startSendDownward/4]).

-define(dis, 100).
-define(VERSION_RANK, version_rank).
-define(LOG_FILE_NAME, "my_log_file.txt").
-define(MY_DODAGs, my_Dodags).
-define(PARENT, parent).
-define(DOWNWARD_DIGRAPH, downwardDigraph).
-define(DOWNWARD_DIGRAPH_FILE, "downward_digraph_file.txt").
-define(ROOT_SERVER, rootServer).

%**************   FIND FRIENDS   **************%

% Find Me and My Friends
findMeAndNeighbors(Me) ->
  MyNode = hd(ets:lookup(nodeList, Me)),
  Y = ets:tab2list(nodeList),
  OtherNodeList = [X || X <- Y, element(1, X) =/= element(1, MyNode)],
  {MyNode, utils:findNeighbors(MyNode, OtherNodeList)}.


% Find Your Neighbors
findNeighbors(MyNodeLocation, NodeLocationList) ->
  [Neighbors || Neighbors <- NodeLocationList, calculateDistance(MyNodeLocation, Neighbors) < ?dis].

% Location = {Pid,X,Y}
calculateDistance({_, {_, X_Node_1, Y_Node_1}}, {_, {_, X_Node_2, Y_Node_2}}) ->
  X_distance = erlang:abs(X_Node_1 - X_Node_2),
  Y_distance = erlang:abs(Y_Node_1 - Y_Node_2),
  math:sqrt(X_distance * X_distance + Y_distance * Y_distance).


%**************   AFTER DIO - CHECK UPDATE   **************%

% Got DIO message - check if the new message was sent from:
% a better rank or a more advanced version
checkIfUpdateNeeded(DodagId, NewVersion, NewRank, From) ->
  case get({?VERSION_RANK, DodagId}) of
    undefined -> % NEED TO UPDATE
      true;
    {PrevVersion, PrevRank} ->
% Check if update is needed
      checkWithPrev(NewVersion, NewRank, PrevVersion, PrevRank, From, DodagId)
  end.

% Compare new info vs previous info
checkWithPrev(NewVersion, NewRank, PrevVersion, PrevRank, From, DodagId) ->
  if
    NewVersion > PrevVersion -> true; % update needed - new version
    true -> if
              NewRank < PrevRank -> true; % update needed - improve rank
              true -> false % no update - same version, not better
            end
  end.

%**************  BUILD DIGRAPH   **************%

requestParent(From, DodagId, NodeList) ->
  lists:foreach(fun(Element) ->
    gen_server:cast(element(1, Element), {requestParent, From, DodagId})
  %   element(1, Element) ! {requestParent, From, DodagId}
                end, NodeList).

% Add all the vertices to the Dodag, even the ones who aren't in the dodag
buildVertexDigraph(NodeList) ->
  Graph = digraph:new(),
  %LABEL = {DODAG_ID}
  lists:foreach(fun(Element) -> digraph:add_vertex(Graph, element(1, Element), {self()}) end, NodeList),
  Graph.

%**************  SENDING MESSASGE   **************%

sendMessage(From, To, Msg) ->
  case checkBestRoute(From, To) of
    {ok, MinDodagId, MinDistance, DodagIdList} ->
      io:format("Best Dodag is: ~p , the distance is: ~p between all dodags:~p~n", [MinDodagId, MinDistance, DodagIdList]),
      if
        From =:= MinDodagId ->
          startSendDownward(From, To, Msg, MinDodagId);
        true ->
          Parent = get({?PARENT, MinDodagId}),
          gen_server:cast(Parent, {parentMsg, From, To, MinDodagId, Msg})
      end;
    {error, Reason} ->
      io:format("NO ROUTE FROM:~p TO:~p~n", [From, To])
  end.


%TODO - fill this correctly
checkBestRoute(From, To) ->
  DodagIdList = getDodagList(),
  case DodagIdList of
    [] -> {error, noRoute};
    _ ->
      io:format("finding the Minimum Path, From: ~p, To:~p: ,DodagList:~p~n", [From, To, DodagIdList]),
      {MinDodagId, MinDistance} = findMinimum(DodagIdList, To, From, {100000, 100000}),
      if
        MinDistance =:= 100000 -> {error, noRoute};
        true ->
          {ok, MinDodagId, MinDistance, DodagIdList}
      end
  end.

findMinimum([], To, From, {MinDodagId, MinDistance}) -> {MinDodagId, MinDistance};
findMinimum(DodagIdList, To, From, {MinDodagId, MinDistance}) ->
  if
    From =:= hd(DodagIdList) -> Distance = calculatePath(To);
    true -> Distance = gen_server:call(hd(DodagIdList), {calculateRoute, To})
  end,
  {_, Rank} = get({?VERSION_RANK, hd(DodagIdList)}),
  if
    Distance =:= false -> findMinimum(tl(DodagIdList), To, From, {MinDodagId, MinDistance});
    Distance + Rank < MinDistance -> findMinimum(tl(DodagIdList), To, From, {hd(DodagIdList), Distance + Rank});
    true -> findMinimum(tl(DodagIdList), To, From, {MinDodagId, MinDistance})
  end.

%**************  TODO - think about it   **************%

getDodagList() ->
  case get(?MY_DODAGs) of
    undefined -> % NEED TO UPDATE
      io:format("NO DODAGLIST ~p~n", [self()]),
      [];
    DodagList ->
      DodagList
  end.


calculatePath(To) ->
  case digraph:get_path(get(?DOWNWARD_DIGRAPH), self(), To) of
    false -> if
               To =:= self() -> 0;
               true -> false
             end;
    PathList -> length(PathList)
  end.

startSendDownward(From, To, Msg, MinDodagId) ->
  PathList = digraph:get_path(get(?DOWNWARD_DIGRAPH), self(), To),
  io:format("PathList: ~p~n", [PathList]),
  gen_server:cast(hd(PathList), {downwardMessage, From, To, Msg, MinDodagId, tl(PathList)}).
