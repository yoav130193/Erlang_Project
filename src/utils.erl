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
-export([findMeAndNeighbors/1, handleDownwardMessage/6, findNeighbors/2, checkIfUpdateNeeded/4, requestParent/3, buildVertexDigraph/1, sendMessageNonStoring/3, sendMessageStoring/3, deleteMessageFromEts/4, getDodagList/0, calculatePath/1, startSendDownward/5]).

-define(dis, 100).
-define(VERSION_RANK, version_rank).
-define(LOG_FILE_NAME, "my_log_file.txt").
-define(MY_DODAGs, my_Dodags).
-define(PARENT, parent).
-define(DOWNWARD_DIGRAPH, downwardDigraph).
-define(DOWNWARD_DIGRAPH_FILE, "downward_digraph_file.txt").
-define(ROOT_SERVER, rootServer).
-define(MSG_TABLE, msgTable).
-define(RPL_SERVER, rplServer).
-define(RPL_REF, rplRef).


-record(msg_table_key, {dodagId, from, to}).

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
    ets:insert(?MSG_TABLE, {#msg_table_key{dodagId = DodagId, from = self(), to = element(1, Element)}, {msg}}),
    gen_server:cast(element(1, Element), {requestParent, From, DodagId}) end, NodeList).

% Add all the vertices to the Dodag, even the ones who aren't in the dodag
buildVertexDigraph(NodeList) ->
  Graph = digraph:new(),
  lists:foreach(fun(Element) -> digraph:add_vertex(Graph, element(1, Element), {self()}) end, NodeList),
  Graph.

%**************  SENDING MESSASGE   **************%

sendMessageNonStoring(From, To, Msg) ->
  case checkBestRoute(From, To) of
    {ok, MinDodagId, MinDistance, MinPath, DodagIdList} ->
      io:format("Best Dodag is: ~p , the distance is: ~p the path from the root is: ~p, between all dodags:~p~n", [MinDodagId, MinDistance, MinPath, DodagIdList]),
      if
        From =:= MinDodagId ->
          startSendDownward(From, To, Msg, MinDodagId, []);
        true ->
          Parent = get({?PARENT, MinDodagId}),
          gen_server:cast(Parent, {parentMsg, From, To, MinDodagId, Msg, []})
      end;
    {error, Reason} ->
      io:format("NO ROUTE FROM:~p TO:~p~n", [From, To]),
      gen_server:reply(element(2, hd(ets:lookup(?RPL_REF, ref))), [])


  end.

sendMessageStoring(From, To, Msg) ->
  From.


%TODO - fill this correctly
checkBestRoute(From, To) ->
  DodagIdList = getDodagList(),
  case DodagIdList of
    [] -> {error, noRoute};
    _ ->
      io:format("finding the Minimum Path, From: ~p, To: ~p ,DodagList: ~p~n", [From, To, DodagIdList]),
      {MinDodagId, MinDistance, MinPath} = findMinimum(DodagIdList, To, From, {100000, 100000, []}),
      if
        MinDistance =:= 100000 -> {error, noRoute};
        true ->
          {ok, MinDodagId, MinDistance, MinPath, DodagIdList}
      end
  end.

findMinimum([], To, From, {MinDodagId, MinDistance, MinPath}) -> {MinDodagId, MinDistance, MinPath};
findMinimum(DodagIdList, To, From, {MinDodagId, MinDistance, MinPath}) ->
  if
    From =:= hd(DodagIdList) -> {Path, Distance} = calculatePath(To);
    true -> {Path, Distance} = gen_server:call(hd(DodagIdList), {calculateRoute, To})
  end,
  {_, Rank} = get({?VERSION_RANK, hd(DodagIdList)}),
  if
    Distance =:= false -> findMinimum(tl(DodagIdList), To, From, {MinDodagId, MinDistance, MinPath});
    Distance + Rank < MinDistance -> findMinimum(tl(DodagIdList), To, From, {hd(DodagIdList), Distance + Rank, Path});
    true -> findMinimum(tl(DodagIdList), To, From, {MinDodagId, MinDistance, MinPath})
  end.

%**************  TODO - think about it   **************%

getDodagList() ->
  case get(?MY_DODAGs) of
    undefined -> % NEED TO UPDATE
      %  io:format("NO DODAGLIST ~p~n", [self()]),
      [];
    DodagList ->
      DodagList
  end.


calculatePath(To) ->
  case digraph:get_path(get(?DOWNWARD_DIGRAPH), self(), To) of
    false -> if
               To =:= self() -> {self(), 0};
               true -> {ok, false}
             end;
    PathList -> {PathList, length(PathList)}
  end.

startSendDownward(From, To, Msg, MinDodagId, Path) ->
  PathList = digraph:get_path(get(?DOWNWARD_DIGRAPH), self(), To),
  io:format("Me: ~p, PathList: ~p~n", [self(), PathList]),
  gen_server:cast(hd(PathList), {downwardMessage, From, To, Msg, MinDodagId, tl(PathList), Path ++ [self()]}).

handleDownwardMessage(DodagID, Msg, From, To, WholePath, PathList) ->
  MyPid = self(),
  case To of
    MyPid ->
      io:format("Got the Msg!!! DodagID: ~p myNode: ~p msg: ~p, From: ~p, To: ~p Path: ~p~n", [DodagID, self(), Msg, From, To, WholePath ++ [self()]]),
      gen_server:reply(element(2, hd(ets:lookup(?RPL_REF, ref))), WholePath ++ [self()]);
    _ ->
      io:format("downwardMessage, DodagID: ~p myNode: ~p msg: ~p, From: ~p, To: ~p TempPath: ~p~n", [DodagID, self(), Msg, From, To, WholePath]),
      gen_server:cast(hd(PathList), {downwardMessage, From, To, Msg, DodagID, tl(PathList), WholePath})
  end.

deleteMessageFromEts(DodagId, From, To, Action) ->
  ets:delete(?MSG_TABLE, #msg_table_key{dodagId = DodagId, from = From, to = To}),
  case ets:tab2list(?MSG_TABLE) of
    [] -> io:format("finish Building~n~n"),
      gen_server:cast(rplServer, Action);
    List -> %io:format("not finished yet, list left: ~p~n", [List])
      continue
  end.