%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2020 11:18
%%%-------------------------------------------------------------------
-module(rplServer).
-author("yoavlevy").
-behavior(gen_server).
%% API
-export([start_link/1,
  code_change/3,
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

-define(LOG_FILE_NAME, "my_log_file.txt").
-define(DOWNWARD_DIGRAPH_FILE, "downward_digraph_file.txt").
-define(VERSION_RANK, version_rank).

-define(RPL_SERVER, rplServer).
-define(NODE_SERVER, nodeServer).
-define(ROOT_SERVER, rootServer).


-define(NODE_LIST, nodeList).
-define(ROOT_LIST, rootList).



start_link(Mop) -> gen_server:start_link({local, ?RPL_SERVER}, ?RPL_SERVER, [Mop], []).


%*****************   Initialization    ***************%

% Initialize all information to start the program
init(Mop) ->
  % FOR DEBUG ONLY
  {ok, S} = file:open(?LOG_FILE_NAME, [append]),
  io:format(S, "~s~n", ["{DODAG_ID,Message Type,From,To}"]),

  process_flag(trap_exit, true),
  io:format("rplServer init~n"),
  ets:new(?NODE_LIST, [set, named_table, public]),
  ets:new(?ROOT_LIST, [set, named_table, public]),
  ets:new(mop, [set, named_table, public]),
  ets:insert(mop, {mopKey, Mop}),
  NodeCount = 1,
  RootCount = 1,
  random:seed(1),
  RandomLocationList = [random:uniform(200) || _ <- lists:seq(1, 50)],
  {ok, {NodeCount, RootCount, RandomLocationList}}.


%*****************   CALLS FROM GUI    ***************%

%*** ADD NODE ***%

% Call from the GUI - addition of a new node
% Return to the GUI the Pid of the new node
%TODO - handle REF
handle_call({addNode, normal}, _From, {NodeCount, RootCount, RandomLocationList}) ->
  io:format("myserver wants to add a normal node~n"),
  {_, Pid} = nodeServer:start_link(NodeCount),
  Ref = 0,
  % {Pid, Ref} = spawn_monitor(nodeLoop, loopStart, [NodeCount]),
  ets:insert(?NODE_LIST, {Pid, {Ref, hd(RandomLocationList), hd(tl(RandomLocationList))}}),
  NewData = {NodeCount + 1, RootCount, tl(tl(RandomLocationList))},
  {reply, {Pid, Ref}, NewData};

% CALL from the GUI - to add a root node
% Return to the GUI the Pid of the new node
%TODO - handle REF
handle_call({addNode, root}, _From, {NodeCount, RootCount, RandomLocationList}) ->
  io:format("myserver wants to add a root node~n"),
  {_, Pid} = rootServer:start_link(RootCount),
  Ref = 0,
%  {Pid, Ref} = spawn_monitor(rootLoop, loopStart, [RootCount]),
  ets:insert(?NODE_LIST, {Pid, {Ref, hd(RandomLocationList), hd(tl(RandomLocationList))}}),
  ets:insert(?ROOT_LIST, {Pid, {Ref, hd(RandomLocationList), hd(tl(RandomLocationList))}}),
  NewData = {NodeCount + 1, RootCount + 1, tl(tl(RandomLocationList))},

  {reply, {Pid, Ref}, NewData}.

%*** Nodes Locations ***%

% CAST from the GUI - request to send message from node A -> B
% no reply to the GUI
% server sends to all the roots to start building the network
% After the network is ready -> try send a message
handle_cast({buildNetwork}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  buildNetwork(RootList),
  %From ! {message, To, Msg},
%  sendLocations(RootList, Locations),
  {noreply, Data};

handle_cast({downwardDigraphBuild}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  upwardDigraphBuild(RootList),
  %From ! {message, To, Msg},
%  sendLocations(RootList, Locations),
  {noreply, Data};

%TODO - just for debug
handle_cast({getAllPath}, Data) ->
  RootList = ets:tab2list(rootList),
  getAllPath(RootList),
  %upwardDigraphBuild(RootList),
  %From ! {message, To, Msg},
%  sendLocations(RootList, Locations),
  {noreply, Data};

% CAST from the GUI - request to send message from node A -> B
% no reply to the GUI
% server sends to all the roots to start building the network
% After the network is ready -> try send a message
handle_cast({sendMessage, From, To, Msg}, Data) ->
  gen_server:cast(From, {sendMessage, From, To, Msg}),
  %buildNetwork(RootList),
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

% Send to all the roots to start build its DODAGs
buildNetwork(RootList) ->
  lists:foreach(fun(Element) ->
    gen_server:cast(element(1, Element), {buildNetwork})
  %   element(1, Element) ! {buildNetwork}
                end, RootList).

% Building from the roots the downwardDigraphBuild
upwardDigraphBuild(RootList) ->
  lists:foreach(fun(Element) ->
    gen_server:cast(element(1, Element), {downwardDigraphBuild})
%element(1, Element) ! {downwardDigraphBuild}
                end, RootList).

% Just for Debug, Check all the Path Exist
getAllPath(RootList) ->
  lists:foreach(fun(Element) ->
    gen_server:cast(element(1, Element), {getAllPath})
  %element(1, Element) ! {getAllPath}
                end, RootList).
