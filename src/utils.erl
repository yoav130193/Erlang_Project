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
-include("include/header.hrl").


%% API
-export([findMeAndNeighbors/1, handleDownwardMessage/6, findNeighbors/2,
  checkIfUpdateNeeded/4, requestParent/3, buildVertexDigraph/1, sendMessageNonStoring/4,
  sendMessageStoring/4, deleteMessageFromEts/5, getDodagList/0, calculatePath/3, startSendMessageDownward/6]).


%**************   FIND FRIENDS   **************%

% Find Me and My Friends
findMeAndNeighbors(Me) ->
  MyNode = hd(ets:lookup(nodeList, Me)),
  Y = ets:tab2list(nodeList),
  io:format("utils ETS table: ~p~n",[Y]),
  OtherNodeList = [X || X <- Y, element(1, X) =/= element(1, MyNode)],
  {MyNode, utils:findNeighbors(MyNode, OtherNodeList)}.


% Find Your Neighbors
findNeighbors(MyNodeLocation, NodeLocationList) ->
  [Neighbors || Neighbors <- NodeLocationList, calculateDistance(MyNodeLocation, Neighbors) < ?dis].

% Location = {Pid,X,Y}
%{Pid,{Ref,NumOfRoots,RootOrNode,Func,Type,?incerement,{X,Y},normal}})
%{_,{_,_,_,_,_,_,{X_Node_1,Y_Node_1},_}}
%%{_,{_,_,_,_,_,_,{X_Node_2,Y_Node_2},_}}
calculateDistance({_,{_,_,_,_,_,_,{X_Node_1,Y_Node_1},_}}, {_,{_,_,_,_,_,_,{X_Node_2,Y_Node_2},_}}) ->
  X_distance = erlang:abs(X_Node_1 - X_Node_2),
  Y_distance = erlang:abs(Y_Node_1 - Y_Node_2),
  math:sqrt(X_distance * X_distance + Y_distance * Y_distance).


%**************   AFTER DIO - CHECK UPDATE   **************%

% Got DIO message - check if the new message was sent from:
% a better rank or a more advanced version
checkIfUpdateNeeded(DodagId, NewVersion, NewRank, From) ->
  case get({?VERSION_RANK, DodagId}) of
    undefined -> ?Update; % NEED TO UPDATE
    {PrevVersion, PrevRank} -> checkWithPrev(NewVersion, NewRank, PrevVersion, PrevRank, From, DodagId)
  end.

% Compare new Version and Rank vs Previous Version and Rank
checkWithPrev(NewVersion, NewRank, PrevVersion, PrevRank, From, DodagId) ->
  if
    NewVersion > PrevVersion -> ?Update; % update needed - new version
    true -> if
              NewRank < PrevRank -> ?Update; % update needed - improve rank
              NewRank =:= PrevRank -> ?Update_Addition;
              true -> ?Update_NO % no update - same version, not better
            end
  end.

%**************  BUILD DIGRAPH   **************%

requestParent(From, DodagId, NodeList) ->
  lists:foreach(fun(Element) ->
    ets:insert(?MSG_TABLE, {#msg_table_key{dodagId = DodagId, from = self(), to = element(1, Element)}, {msg}}),
    gen_server:cast(element(1, Element), {requestParent, From, DodagId})
                end, NodeList).

% Add all the vertices to the Dodag, even the ones who aren't in the dodag
buildVertexDigraph(NodeList) ->
  Graph = digraph:new(),
  lists:foreach(fun(Element) -> digraph:add_vertex(Graph, element(1, Element), {self()}) end, NodeList),
  Graph.

%**************  SENDING MESSASGE   **************%

sendMessageNonStoring(From, To, Msg, Mop) ->
  case checkBestRoute(From, To, Mop) of
    {ok, MinDodagId, MinDistance, MinPath, DodagIdList} ->
      io:format("Best Dodag is: ~p , the distance is: ~p the path from the root is: ~p, between all dodags:~p~n", [MinDodagId, MinDistance, MinPath, DodagIdList]),
      if
        From =:= MinDodagId -> startSendMessageDownward(From, To, Msg, MinDodagId, [], Mop);
        true ->
          Parent = hd(get({?PARENT, MinDodagId})),
          gen_server:cast(Parent, {parentMsg, From, To, MinDodagId, Msg, [self()]})
      end;
    {error, Reason} ->
      io:format("NO ROUTE FROM:~p TO:~p~n", [From, To]),
      gen_server:reply(element(2, hd(ets:lookup(?RPL_REF, ref))), [])
  end.

sendMessageStoring(From, To, Msg, Mop) ->
  case checkBestRouteStoring(From, To, Mop) of
    {ok, MinDodagId, MinDistance, MinPath, DodagIdList} ->
      io:format("Best Dodag is: ~p , the distance is: ~p the path from the root is: ~p, between all dodags:~p~n", [MinDodagId, MinDistance, MinPath, DodagIdList]),
      gen_server:cast(hd(tl(MinPath)), {downwardMessage, From, To, Msg, MinDodagId, tl(MinPath), [self()]});
    % startSendDownward(From, To, Msg, MinDodagId, []);
    {error, Reason} ->
      io:format("NO ROUTE FROM:~p TO:~p~n", [From, To]),
      gen_server:reply(element(2, hd(ets:lookup(?RPL_REF, ref))), [])
  end.

startSendMessageDownward(From, To, Msg, MinDodagId, PathToRoot, Mop) ->
  case hd(Mop) of
    ?NON_STORING -> PathList = digraph:get_short_path(get(?DOWNWARD_DIGRAPH), self(), To);
    ?STORING -> PathList = digraph:get_short_path(element(2, hd(ets:lookup(?DOWNWARD_DIGRAPH, MinDodagId))), self(), To)
  end,
  io:format("Me: ~p, PathList: ~p, PathToRoot: ~p~n", [self(), PathList, PathToRoot]),
  gen_server:cast(hd(tl(PathList)), {downwardMessage, From, To, Msg, MinDodagId, tl(tl(PathList)), PathToRoot ++ [self()]}).

handleDownwardMessage(DodagID, Msg, From, To, TempWholePath, PathList) ->
  MyPid = self(),
  case To of
    MyPid ->
      io:format("Got the Msg!!! DodagID: ~p myNode: ~p msg: ~p, From: ~p, To: ~p TempPath: ~p~n", [DodagID, self(), Msg, From, To, TempWholePath ++ [self()]]),
      gen_server:reply(element(2, hd(ets:lookup(?RPL_REF, ref))), TempWholePath ++ [self()]);
    _ ->
      io:format("downwardMessage, DodagID: ~p myNode: ~p msg: ~p, From: ~p, To: ~p TempPath: ~p PathList: ~p~n", [DodagID, self(), Msg, From, To, TempWholePath,PathList]),
      gen_server:cast(hd(PathList), {downwardMessage, From, To, Msg, DodagID, tl(PathList), TempWholePath ++ [self()]})
  end.

%**************  FINDING BEST ROUTE   **************%

% Checks the best route for Non-Storing mode
checkBestRoute(From, To, Mop) ->
  DodagIdList = getDodagList(),
  case DodagIdList of
    [] -> {error, noRoute};
    _ ->
      io:format("finding the Minimum Path, From: ~p, To: ~p ,DodagList: ~p~n", [From, To, DodagIdList]),
      {MinDodagId, MinDistance, MinPath} = findMinimumNonStoring(DodagIdList, To, From, {100000, 100000, []}, Mop),
      if
        MinDistance =:= 100000 -> {error, noRoute};
        true -> {ok, MinDodagId, MinDistance, MinPath, DodagIdList}
      end
  end.

% Checks the best route for Storing mode
checkBestRouteStoring(From, To, Mop) ->
  DodagIdList = getDodagList(),
  case DodagIdList of
    [] -> {error, noRoute};
    _ ->
      {MinDodagId, MinDistance, MinPath} = findMinimumStoring(DodagIdList, To, From, {100000, 100000, []}, Mop),
      io:format("Minimum Path: ~p, From: ~p, To: ~p ,DodagList: ~p~n", [MinPath, From, To, DodagIdList]),
      if
        MinDistance =:= 100000 -> {error, noRoute};
        true -> {ok, MinDodagId, MinDistance, MinPath, DodagIdList}
      end
  end.

% Find Minimum path at non-storing mode
findMinimumNonStoring([], To, From, {MinDodagId, MinDistance, MinPath}, _Mop) -> {MinDodagId, MinDistance, MinPath};
findMinimumNonStoring(DodagIdList, To, From, {MinDodagId, MinDistance, MinPath}, Mop) ->
  if
    From =:= hd(DodagIdList) -> {Path, Distance} = calculatePath(To, Mop, hd(DodagIdList));
    true -> {Path, Distance} = gen_server:call(hd(DodagIdList), {calculateRoute, To})
  end,
  {_, Rank} = get({?VERSION_RANK, hd(DodagIdList)}),
  if
    Distance =:= false -> findMinimumNonStoring(tl(DodagIdList), To, From, {MinDodagId, MinDistance, MinPath}, Mop);
    Distance + Rank < MinDistance ->
      findMinimumNonStoring(tl(DodagIdList), To, From, {hd(DodagIdList), Distance + Rank, Path}, Mop);
    true -> findMinimumNonStoring(tl(DodagIdList), To, From, {MinDodagId, MinDistance, MinPath}, Mop)
  end.

% Find Minimum path at storing mode
findMinimumStoring([], To, From, {MinDodagId, MinDistance, MinPath}, Mop) -> {MinDodagId, MinDistance, MinPath};
findMinimumStoring(DodagIdList, To, From, {MinDodagId, MinDistance, MinPath}, Mop) ->
  if
    From =:= hd(DodagIdList) -> {Path, Distance} = calculatePath(To, Mop, hd(DodagIdList));
    true ->
      %xPath = digraph:get_path(element(2, hd(ets:lookup(?DOWNWARD_DIGRAPH, hd(DodagIdList)))), self(), To),
      %Distance = length(Path)
      {Path, Distance} = calculatePath(To, Mop, hd(DodagIdList))
  end,
  if
    Distance =:= false -> findMinimumStoring(tl(DodagIdList), To, From, {MinDodagId, MinDistance, MinPath}, Mop);
    Distance < MinDistance -> findMinimumStoring(tl(DodagIdList), To, From, {hd(DodagIdList), Distance, Path}, Mop);
    true -> findMinimumStoring(tl(DodagIdList), To, From, {MinDodagId, MinDistance, MinPath}, Mop)
  end.

%**************  TODO - think about it   **************%

getDodagList() ->
  case get(?MY_DODAGs) of
    undefined -> []; % NEED TO UPDATE
    DodagList -> DodagList
  end.


calculatePath(To, Mop, DodagId) ->
  % Mop = element(2, hd(ets:lookup(mop, mopKey))),
  case hd(Mop) of
    ?NON_STORING -> DownwardDigraph = digraph:get_short_path(get(?DOWNWARD_DIGRAPH), self(), To);
    ?STORING ->
      DownwardDigraph = digraph:get_short_path(element(2, hd(ets:lookup(?DOWNWARD_DIGRAPH, DodagId))), self(), To)
  end,
  case DownwardDigraph of
    false -> if
               To =:= self() -> {self(), 0};
               true -> {ok, false}
             end;
    PathList -> {PathList, length(PathList)}
  end.


deleteMessageFromEts(DodagId, From, To, Action, WhereFrom) ->
  ets:delete(?MSG_TABLE, #msg_table_key{dodagId = DodagId, from = From, to = To}),
  case ets:tab2list(?MSG_TABLE) of
    [] ->
      io:format("Dodag_id: ~p, Node: ~p, From: ~p, To: ~p, finish Building Action: ~p WhereFrom: ~p~n", [DodagId, self(), From, To, Action, WhereFrom]),
      gen_server:cast(rplServer, Action);
    List -> %io:format("not finished yet, list left: ~p~n", [List])
      continue
  end.



