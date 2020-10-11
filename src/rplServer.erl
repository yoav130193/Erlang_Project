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
-include("include/header.hrl").

%% API
-export([start_link/1,
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  init/1, deleteNode/0,
  terminate/2,
  printData/1]).

start_link(Mop) -> gen_server:start_link({local, ?RPL_SERVER}, ?RPL_SERVER, [Mop], []).


%*****************   Initialization    ***************%

% Initialize all information to start the program
init(Mop) ->
% FOR DEBUG ONLY
  {ok, S} = file:open(?LOG_FILE_NAME, [write]),
  io:format(S, "~s~n", ["{DODAG_ID,Message Type,From,To}"]),
  process_flag(trap_exit, true),
  io:format("rplServer init Mop: ~p~n", [Mop]),
  %ets:new(?NODE_LIST, [set, named_table, public]),
  ets:new(?ROOT_LIST, [set, named_table, public]),
  ets:new(?MSG_TABLE, [set, named_table, public]),
  ets:new(?RPL_REF, [set, named_table, public]),
  ets:new(?DOWNWARD_DIGRAPH, [set, named_table, public]),
  NodeCount = 1,
  RootCount = 1,
  random:seed(1),
  RandomLocationList = [random:uniform(200) || _ <- lists:seq(1, 500)],
  Data = #rplServerData{nodeCount = NodeCount, rootCount = RootCount, randomLocationList = RandomLocationList, msg_id = 0, messageList = [], mop = Mop},
  {ok, Data}.


%*****************   CALLS FROM GUI    ***************%

%*** ADD NODE ***%

% Call from the GUI - addition of a new node
% Return to the GUI the Pid of the new node
handle_call({addNode, node}, _From, Data) ->
  %{_, Pid} = nodeServer:start_link({Data#rplServerData.nodeCount, Data#rplServerData.mop}),
  {_, {Pid, Ref}} = nodeServer:start_link({Data#rplServerData.nodeCount, Data#rplServerData.mop}),
%  Pid = 0, Ref = 0,
%  {_, {Pid, Ref}} = rpc:call(?NODE_1, ?NODE_SERVER, start_link, [{Data#rplServerData.nodeCount, Data#rplServerData.mop}]),
  io:format("rplserver wants to add a normal node: ~p~n", [Pid]),
 % ets:insert(?NODE_LIST, {Pid, {Ref, hd(Data#rplServerData.randomLocationList), hd(tl(Data#rplServerData.randomLocationList))}}),
  NewData = updateData(Data#rplServerData.nodeCount + 1, Data#rplServerData.rootCount,
    tl(tl(Data#rplServerData.randomLocationList)), Data#rplServerData.msg_id, Data#rplServerData.messageList, Data#rplServerData.mop),
  {reply, {Pid, Ref}, NewData};

% CALL from the GUI - to add a root node
% Return to the GUI the Pid of the new node
handle_call({addNode, root}, _From, Data) ->
  io:format("rplserver wants to add a root node~n"),
  {_, {Pid, Ref}} = rootServer:start_link({Data#rplServerData.rootCount, Data#rplServerData.mop}),
%  ets:insert(?NODE_LIST, {Pid, {Ref, hd(Data#rplServerData.randomLocationList), hd(tl(Data#rplServerData.randomLocationList))}}),
  %ets:insert(?ROOT_LIST, {Pid, {Ref, hd(Data#rplServerData.randomLocationList), hd(tl(Data#rplServerData.randomLocationList))}}),
  NewData = updateData(Data#rplServerData.nodeCount + 1, Data#rplServerData.rootCount + 1,
    tl(tl(Data#rplServerData.randomLocationList)), Data#rplServerData.msg_id, Data#rplServerData.messageList, Data#rplServerData.mop),
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
  %  deleteNode(),
  RootList = ets:tab2list(?ROOT_LIST),
  downwardDigraphBuild(RootList),


  {noreply, Data};


%*** BUILD DOWNWARD DIGRAPH ***%

% Start Building the digraph
handle_cast({downwardDigraphBuild}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  downwardDigraphBuild(RootList),
  {noreply, Data};

% Symbols that the downward digraph got finished building.
% Now You can send the messages safetly
handle_cast({finishedDigraphBuilding}, Data) ->
  io:format("Can Send Message~n"),
  sendAllMessages(Data#rplServerData.messageList, Data#rplServerData.mop),
  NewData = updateData(Data#rplServerData.nodeCount, Data#rplServerData.rootCount,
    Data#rplServerData.randomLocationList, Data#rplServerData.msg_id, [], Data#rplServerData.mop),
  {noreply, NewData};

%*** SENDING MESSAGES ***%

% UNICAST from the GUI - request to send message from node A -> B
% Without building the network
handle_cast({sendMessage, From, To, Msg}, Data) ->
  case Data#rplServerData.mop of
    ?STORING ->
      io:format("STORING 151~n"),
      gen_server:cast(From, {sendMessageStoring, From, To, Msg});
    ?NON_STORING -> gen_server:cast(From, {sendMessageNonStoring, From, To, Msg})
  end,
  {noreply, Data};

% UNICAST from the GUI - request to send message from node A -> B
% roots start building the network
% After the network is ready -> try send a message
handle_cast({sendUnicastMessage, From, To, Msg}, Data) ->
  RootList = ets:tab2list(?ROOT_LIST),
  NewData = updateData(Data#rplServerData.nodeCount, Data#rplServerData.rootCount,
    Data#rplServerData.randomLocationList, Data#rplServerData.msg_id + 1, Data#rplServerData.messageList ++
    [#messageFormat{msgId = Data#rplServerData.msg_id + 1, from = From, to = To, msg = Msg}], Data#rplServerData.mop),
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

% Just for debug
handle_cast({deleteNode}, Data) ->
  deleteNode(),
  {noreply, Data};


%***************    Examples    *************%
handle_cast(_Request, State) ->
  {reply, State}.


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

% Updating the state data
updateData(NodeCount, RootCount, RandomLocationList, Msg_ID, MessageList, Mop) ->
  #rplServerData{nodeCount = NodeCount, rootCount = RootCount,
    randomLocationList = RandomLocationList, msg_id = Msg_ID, messageList = MessageList, mop = Mop}.


sendAllMessages([], _Mop) -> [];
sendAllMessages(MessageList, Mop) ->
  Message = hd(MessageList),
  case hd(Mop) of
    ?STORING ->
      Path = gen_server:call(Message#messageFormat.from, {sendMessageStoring, Message#messageFormat.from, Message#messageFormat.to, Message#messageFormat.msg}, 100000),
      io:format("RPL Server:,Storing Message, From: ~p, To: ~p , Path: ~p~n~n ", [Message#messageFormat.from, Message#messageFormat.to, Path]),
      wx_object:cast(gfx_server,{messageSent,Message#messageFormat.msgId,Path});
    ?NON_STORING ->
      Path = gen_server:call(Message#messageFormat.from, {sendMessageNonStoring, Message#messageFormat.from, Message#messageFormat.to, Message#messageFormat.msg}, 100000),
      io:format("RPL Server:,NON_STORING Message, From: ~p, To: ~p , Path: ~p~n~n ", [Message#messageFormat.from, Message#messageFormat.to, Path]),
      wx_object:cast(gfx_server,{messageSent,Message#messageFormat.msgId,Path})
  end,
  sendAllMessages(tl(MessageList), Mop).

addMessagesToData([], NewData) -> NewData;
addMessagesToData(MessageList, NewData) ->
  Message = hd(MessageList),
  addMessagesToData(tl(MessageList), updateData(NewData#rplServerData.nodeCount, NewData#rplServerData.rootCount,
    NewData#rplServerData.randomLocationList, NewData#rplServerData.msg_id + 1, NewData#rplServerData.messageList ++
    [#messageFormat{msgId = NewData#rplServerData.msg_id + 1, from = Message#messageFormat.from, to = Message#messageFormat.to, msg = Message#messageFormat.msg}], NewData#rplServerData.mop)).


%***********   FOR DEBUGGING    ************#
deleteNode() ->
  NodeList = ets:tab2list(nodeList),
  exit(element(1, hd(NodeList)), rplExit).


printData(Data) ->
  io:format("RootCount: ~p, NodeCount:~p, RandomList: ~p,Msg_id: ~p,MessageList= ~p~n", [Data#rplServerData.rootCount, Data#rplServerData.nodeCount,
    Data#rplServerData.randomLocationList, Data#rplServerData.msg_id, Data#rplServerData.messageList]).


%***********     Handle Errors    ************#

% Handle falls from root or Node Server
handle_info({'DOWN', Ref, process, Pid, {Info, Reason, ProcState}}, State) ->
  io:format("rplServer Monitor crash, Ref: ~p , Pid: ~p, Info: ~p, Reason: ~p State: ~p~n", [Ref, Pid, Info, Reason, ProcState]),
  sendGfxServerCrashAlert(Pid, Ref, Reason),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.


terminate(Reason, State) ->
  io:format("rplServer terminate, Reason: ~p State: ~p~n", [Reason, State]),
  exit({rplCrash, Reason, State}).


sendGfxServerCrashAlert(Pid, Ref, Reason) ->
  %TODO - Uncomment this and check this
  %gen_server:call(?GFX_SERVER, {element(1, Reason), Pid, Ref, element(2, Reason)}),
  Pid.