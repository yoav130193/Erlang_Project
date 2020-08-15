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
  terminate/2,
  printData/1]).

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
-define(MSG_TABLE, msgTable).


-define(NODE_LIST, nodeList).
-define(ROOT_LIST, rootList).
-define(MOP, mop).

-record(rplServerData, {nodeCount, rootCount, randomLocationList, msg_id, messageList}).
-record(messageFormat, {msgId, from, to, msg}).


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
  ets:new(?MSG_TABLE, [set, named_table, public]),
  ets:new(?MOP, [set, named_table, public]),
  ets:insert(mop, {mopKey, Mop}),
  NodeCount = 1,
  RootCount = 1,
  random:seed(1),
  RandomLocationList = [random:uniform(200) || _ <- lists:seq(1, 50)],
  Data = #rplServerData{nodeCount = NodeCount, rootCount = RootCount, randomLocationList = RandomLocationList, msg_id = 0, messageList = []},
  {ok, Data}.


%*****************   CALLS FROM GUI    ***************%

%*** ADD NODE ***%

% Call from the GUI - addition of a new node
% Return to the GUI the Pid of the new node
%TODO - handle REF
handle_call({addNode, normal}, _From, Data) ->
  io:format("myserver wants to add a normal node~n"),
  {_, Pid} = nodeServer:start_link(Data#rplServerData.nodeCount),
  Ref = 0,
  ets:insert(?NODE_LIST, {Pid, {Ref, hd(Data#rplServerData.randomLocationList), hd(tl(Data#rplServerData.randomLocationList))}}),
  NewData = updateData(Data#rplServerData.nodeCount + 1, Data#rplServerData.rootCount,
    tl(tl(Data#rplServerData.randomLocationList)), Data#rplServerData.msg_id, Data#rplServerData.messageList),
  {reply, {Pid, Ref}, NewData};

% CALL from the GUI - to add a root node
% Return to the GUI the Pid of the new node
%TODO - handle REF
handle_call({addNode, root}, _From, Data) ->
  io:format("myserver wants to add a root node~n"),
  {_, Pid} = rootServer:start_link(Data#rplServerData.rootCount),
  Ref = 0,
  ets:insert(?NODE_LIST, {Pid, {Ref, hd(Data#rplServerData.randomLocationList), hd(tl(Data#rplServerData.randomLocationList))}}),
  ets:insert(?ROOT_LIST, {Pid, {Ref, hd(Data#rplServerData.randomLocationList), hd(tl(Data#rplServerData.randomLocationList))}}),
  NewData = updateData(Data#rplServerData.nodeCount + 1, Data#rplServerData.rootCount + 1,
    tl(tl(Data#rplServerData.randomLocationList)), Data#rplServerData.msg_id, Data#rplServerData.messageList),
  {reply, {Pid, Ref}, NewData}.

%*** BUILD NETWORK ***%

% CAST from the GUI - request to send message from node A -> B
% no reply to the GUI
% server sends to all the roots to start building the network
handle_cast({buildNetwork}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  buildNetwork(RootList),
  {noreply, Data};

% Symbols that the network got finished building.
% Now Build the downward Digraph
% When finished, need to send the message
handle_cast({finishedBuilding}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  downwardDigraphBuild(RootList),
  {noreply, Data};


%*** BUILD DOWNWARD DIGRAPH ***%

handle_cast({downwardDigraphBuild}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  downwardDigraphBuild(RootList),
  {noreply, Data};

% Symbols that the downward digraph got finished building.
% Now You can send the messages safetly
handle_cast({finishedDigraphBuilding}, Data) ->
  io:format("Can Send Message~n"),
  printData(Data),
  sendAllMessages(Data#rplServerData.messageList),
  NewData = updateData(Data#rplServerData.nodeCount, Data#rplServerData.rootCount,
    Data#rplServerData.randomLocationList, Data#rplServerData.msg_id, []),
  {noreply, NewData};

%*** SENDING MESSAGES ***%

% UNICAST from the GUI - request to send message from node A -> B
% Without building the network
handle_cast({sendMessage, From, To, Msg}, Data) ->
  gen_server:cast(From, {sendMessage, From, To, Msg}),
  {noreply, Data};

% UNICAST from the GUI - request to send message from node A -> B
% roots start building the network
% After the network is ready -> try send a message
handle_cast({sendUnicastMessage, From, To, Msg}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  NewData = updateData(Data#rplServerData.nodeCount, Data#rplServerData.rootCount,
    Data#rplServerData.randomLocationList, Data#rplServerData.msg_id + 1, Data#rplServerData.messageList ++
    [#messageFormat{msgId = Data#rplServerData.msg_id + 1, from = From, to = To, msg = Msg}]),
  buildNetwork(RootList),
  {noreply, NewData};

% MULTICAST from the GUI - request to send message from node A -> Group Of Nodes
% roots start building the network
% After the network is ready -> try send a message
% MessageList format  = List of records of messageFormat (above)
handle_cast({sendMulticastMessage, MessageList}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  NewData = addMessagesToData(MessageList, Data),
  buildNetwork(RootList),
  {noreply, NewData};


%*** DEBUG ***%

% Just for debug
handle_cast({getAllPath}, Data) ->
  RootList = ets:tab2list(rootList),
  getAllPath(RootList),
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

% CAST Function
% Send to all the roots to start build its DODAGs
buildNetwork(RootList) ->
  lists:foreach(fun(Element) ->
    gen_server:cast(element(1, Element), {buildNetwork}) end, RootList).

% Building from the roots the downwardDigraphBuild
downwardDigraphBuild(RootList) ->
  lists:foreach(fun(Element) ->
    gen_server:cast(element(1, Element), {downwardDigraphBuild}) end, RootList).

% Just for Debug, Check all the Path Exist
getAllPath(RootList) ->
  lists:foreach(fun(Element) ->
    gen_server:cast(element(1, Element), {getAllPath}) end, RootList).

% Returns the pid of the root list
returnPidList([], RootAcc) -> RootAcc;
returnPidList(RootList, RootAcc) ->
  returnPidList(tl(RootList), [element(1, hd(RootList)) | RootAcc]).

updateData(NodeCount, RootCount, RandomLocationList, Msg_ID, MessageList) ->
  #rplServerData{nodeCount = NodeCount, rootCount = RootCount,
    randomLocationList = RandomLocationList, msg_id = Msg_ID, messageList = MessageList}.

printData(Data) ->
  io:format("RootCount: ~p, NodeCount:~p, RandomList: ~p,Msg_id: ~p,MessageList= ~p~n", [Data#rplServerData.rootCount, Data#rplServerData.nodeCount,
    Data#rplServerData.randomLocationList, Data#rplServerData.msg_id, Data#rplServerData.messageList]).

sendAllMessages([]) -> [];
sendAllMessages(MessageList) ->
  Message = hd(MessageList),
  gen_server:cast(Message#messageFormat.from, {sendMessage, Message#messageFormat.from, Message#messageFormat.to, Message#messageFormat.msg}),
  sendAllMessages(tl(MessageList)).

addMessagesToData([], NewData) -> NewData;
addMessagesToData(MessageList, NewData) ->
  Message = hd(MessageList),
  addMessagesToData(tl(MessageList), updateData(NewData#rplServerData.nodeCount, NewData#rplServerData.rootCount,
    NewData#rplServerData.randomLocationList, NewData#rplServerData.msg_id + 1, NewData#rplServerData.messageList ++
    [#messageFormat{msgId = NewData#rplServerData.msg_id + 1, from = Message#messageFormat.from, to = Message#messageFormat.to, msg = Message#messageFormat.msg}])).
