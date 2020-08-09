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
-export([findMeAndNeighbors/1, findNeighbors/2, checkIfUpdateNeeded/4]).

-define(dis, 100).
-define(VERSION_RANK, version_rank).
-define(LOG_FILE_NAME, "my_log_file.txt").

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
  {ok, S} = file:open(?LOG_FILE_NAME, [append]),
  io:format(S, "~p Check update: DODAG_ID: ~p, NewVersion:~p, NewRank:~p From ~p~n", [self(), DodagId, NewVersion, NewRank, From]),
  case get({?VERSION_RANK, DodagId}) of
    undefined -> % NEED TO UPDATE
      true;
    {PrevVersion, PrevRank} ->
% Check if update is needed
      checkWithPrev(NewVersion, NewRank, PrevVersion, PrevRank, From, DodagId)
  end.

% Compare new info vs previous info
checkWithPrev(NewVersion, NewRank, PrevVersion, PrevRank, From, DodagId) ->
  {ok, S} = file:open(?LOG_FILE_NAME, [append]),
  io:format(S, "~p Continue Check update: DODAG_ID: ~p, From ~p, NewVersion:~p, NewRank:~p, PrevVersion:~p,PrevRank:~p  ~n", [self(), DodagId, From, NewVersion,
    NewRank, PrevVersion, PrevRank]),
  if
    NewVersion > PrevVersion -> true; % update needed - new version
    true -> if
              NewRank < PrevRank -> true; % update needed - improve rank
              true -> false % no update - same version, not better
            end
  end.


