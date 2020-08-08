%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2020 11:18
%%%-------------------------------------------------------------------
-module(myServer).
-author("yoavlevy").
-behavior(gen_server).
%% API
-export([start_link/0]).
-export([code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  init/1,
  terminate/2]).

%******** DATA STRUCTURES ********%
% {nodePid,ref, X , Y}
-type nodeList() :: [{nodePid, ref, float, float}].
% {rootPid,ref, X, Y}
-type rootList() :: [{rootPid, ref, float, float}].

start_link() -> gen_server:start_link(?MODULE, [], []).


%*****************   Initialization    ***************%

% Initialize all information to start the program
init(Data) ->
  process_flag(trap_exit, true),
  io:format("myserver init~n"),
  ets:new(nodeList, [set, named_table]),
  ets:new(rootList, [set, named_table]),
  RootCount = 1,
  {ok, {[], [], RootCount}}.


%*****************   CALLS FROM GUI    ***************%

%*** ADD NODE ***%

% Call from the GUI - addition of a new node
% Return to the GUI the Pid of the new node
handle_call({addNode, normal}, _From, {RootList, NodeList, RootCount}) ->
  io:format("myserver wants to add a normal node~n"),
  {Pid, Ref} = spawn_monitor(nodeLoop, loopStart, []),
  NewData = {RootList, [{Pid, {Ref, 0, 0}} | NodeList], RootCount},
  ets:insert(nodeList, {Pid, {Ref, 0, 0}}),
  {reply, Pid, NewData};

% CALL from the GUI - to add a root node
% Return to the GUI the Pid of the new node
handle_call({addNode, root}, _From, {RootList, NodeList, RootCount}) ->
  io:format("myserver wants to add a root node~n"),
  {Pid, Ref} = spawn_monitor(rootLoop, loopStart, [RootCount]),
  NewData = {[{Pid, {Ref, 0, 0}} | RootList], [{Pid, {Ref, 0, 0}} | NodeList], RootCount + 1},
  ets:insert(nodeList, {Pid, {Ref, 0, 0}}),
  ets:insert(rootList, {Pid, {Ref, 0, 0}}),
  {reply, Pid, NewData}.

%*** Nodes Locations ***%

% CAST from the GUI - request to send message from node A -> B
% no reply to the GUI
% server sends to all the roots to start building the network
% After the network is ready -> try send a message
handle_cast({message, {From, To, Msg}}, Data) ->
  RootList = ets:tab2list(rootList),
  buildNetwork(RootList),
  From ! {message, To, Msg},
%  sendLocations(RootList, Locations),
  {noreply, Data};

%***************    CALLS FROM Nodes    *************%


%***************    Examples    *************%


handle_cast(_Request, State) ->
  {reply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("myserver terminate~n"),
  [].


%TODO - change it if necessary
code_change(_OldVsn, _State, _Extra) ->
  {error, "NYI"}.


%******************   Utils Functions   ************

% Send all locations to a List of Nodes
sendLocations([], Locations) -> [];
sendLocations(NodeList, Locations) ->
  io:format("server sends to: ~p~n ", [element(1, element(1, hd(NodeList)))]),
  element(1, element(1, hd(NodeList))) ! {locations, Locations},
  sendLocations(tl(NodeList), Locations).


% Send all the root to start build its DODAGs
buildNetwork(RootList) ->
  element(1, element(1, hd(RootList))) ! {buildNetwork},
  erlang:error(not_implemented).