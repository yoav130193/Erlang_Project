%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2020 3:55 PM
%%%-------------------------------------------------------------------
-module(gfx_server).
-author("amir").

-behaviour(wx_object).


-import(util,[integer_to_atom/1]).
-import(funcGenerator,[generateSin/2,generatePolynom/2,solveP/3,solveS/3]).
-export([start/1,
         init/1,
         terminate/2,
         code_change/3,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         handle_event/2,
         handle_sync_event/3,
         start_global/1,
         start_global/2
]).
-include_lib("wx/include/wx.hrl").
-include("include/header.hrl").

-define(MyServer,?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% brief - starts gfx application in global mode
% param - Node on which the gfx server runs on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_global(_Node,ProcState) ->
  io:format("Gfx server starting in global mode~n"),
  wx_object:start_link({global,?MyServer},?MODULE,[ProcState],[]).
start_global(Node) ->
  io:format("Gfx server starting in global mode~n"),
  wx_object:start_link({global,?MyServer},?MODULE,[global,Node],[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% brief - starts gfx application in local mode
% param - Node on which the gfx server runs on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Node) ->
  wx_object:start_link({local,?MyServer},?MODULE,[local,Node],[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% brief - initializes the wx_object
% param - Node = the node the object will run on, Mode = local || global
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(State = #state{}) ->
  gen_server:cast({global,?APP_SERVER},drawGFX),
  {State#state.frame,State};

init([Mode,Node]) ->
  process_flag(trap_exit, true),
  ets:new(?pathEts,[set, named_table, public]),
  InitState = init_layout(Mode,Node),
  io:format("gfx done init ~n"),
  {InitState#state.frame,InitState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% brief - wx repaint event called by refresh
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_sync_event(#wx{event=#wxPaint{}}, _, State) ->
  gen_server:call({global,?M_NODE},checkConnections),
  draw(State),
  ok.


%% event by pressing start btn
handle_event(#wx{obj  = StartBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{storing_checkbox = StoringCheckBox,startBtn = StartBtn,nonStoring_checkbox = _NStoringCheckBox}) ->
  Flag = wxCheckBox:isChecked(StoringCheckBox),
  if
    Flag == true ->
      Argument = ?STORING;
    true ->
      Argument = ?NON_STORING
  end,
  Response = case utils:getCorrectNodeToSpawn(gfx) of
     ?R_NODE -> rpc:call('r_node@amirs-MacBook-Pro',rootWrapper,startRPL,[Argument]);
     ?G_NODE -> rpc:call('g_node@amirs-MacBook-Pro',rplWrapper,startRPL,[Argument]);
     ?N_NODE -> rpc:call('n_node@amirs-MacBook-Pro',nodeWrapper,startRPL,[Argument]);
     ?M_NODE -> rplServer:start_link(Argument)
  end ,
  io:format("start response ~p~n",[Response]),
  wxButton:disable(StartBtn),
  {noreply,State};

%% event by choosing Storing Mode
handle_event(#wx{obj  = StoringCheckBox, event = #wxCommand{type = command_checkbox_clicked}},
    State = #state{storing_checkbox = StoringCheckBox,startBtn = StartBtn,nonStoring_checkbox = NStoringCheckBox}) ->
  Flag = wxCheckBox:isChecked(StoringCheckBox),
  if
    Flag == true ->
      wxCheckBox:set3StateValue(NStoringCheckBox,?wxCHK_UNCHECKED),
      wxButton:enable(StartBtn);
    true ->
      StartFlag = wxCheckBox:isChecked(NStoringCheckBox),
      if
        StartFlag == true -> wxButton:enable(StartBtn);
        true -> wxButton:disable(StartBtn)
      end
  end,
  {noreply,State};

%% event by choosing none Storing Mode
handle_event(#wx{obj  = NStoringCheckBox, event = #wxCommand{type = command_checkbox_clicked}},
    State = #state{storing_checkbox = StoringCheckBox,startBtn = StartBtn,nonStoring_checkbox = NStoringCheckBox}) ->
  Flag = wxCheckBox:isChecked(NStoringCheckBox),
  if
    Flag == true ->
      wxCheckBox:set3StateValue(StoringCheckBox,?wxCHK_UNCHECKED),
      wxButton:enable(StartBtn);
    true ->
      StartFlag = wxCheckBox:isChecked(StoringCheckBox),
      if
        StartFlag == true -> wxButton:enable(StartBtn);
        true -> wxButton:disable(StartBtn)
      end
  end,
  {noreply,State};


%% event by change to moveType
handle_event(#wx{obj = MovementList, event = #wxCommand{type = command_combobox_selected}},
    State = #state{node = _Node,frame = _Frame,movementList = MovementList, newRootBtn = NewRootBtn, newNodeBtn = NewNodeBtn
    }) ->
  Value = wxComboBox:getValue(MovementList),
  if
    Value == "" ->
      wxButton:disable(NewRootBtn),
      wxButton:disable(NewNodeBtn),
      {noreply,State} ;
    true ->
      wxButton:enable(NewRootBtn),
      wxButton:enable(NewNodeBtn),
      %wxWindow:refresh(Frame),
      {noreply, State#state{moveType = Value}}
  end;

%%event for selecting a node\root from Q1
handle_event(#wx{obj = Q1_Node_List, event = #wxCommand{type = command_listbox_selected}},
    State = #state{node_list_q_1 = Q1_Node_List,msgState = MsgState,destinations = DstMap,destinationCombobox = DestinationComboBox,srcTextBox = SrcTextBox}) ->
  case MsgState of
    notStarted ->
      NewSrc = wxListBox:getStringSelection(Q1_Node_List),
      PidString = queueKeyToPid(NewSrc,0),
      [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
      [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
      gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},src}}}),
      NewMsgState = srcSelected,
      NewState = State#state{src = Pid,msgState = NewMsgState},
      SrcText = "Source: " ++ NewSrc,
      wxTextCtrl:changeValue(SrcTextBox,SrcText),
      %wxStaticText:setLabel(SrcTextBox,"Source: " ++ pid_to_string(Pid)),
      wxButton:enable(State#state.removeSrcBtn),
      {noreply, NewState};
    srcSelected ->
      NewDst = wxListBox:getStringSelection(Q1_Node_List),
      if
        State#state.src == NewDst ->
          io:format("wrong choice mate ~n"),
          {noreply,State};
        true ->
          %%TODO: change the key that is returned to an actual PID in the real version
          PidString = queueKeyToPid(NewDst,0),
          [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
          wxButton:enable(State#state.sendMsg),
          wxButton:enable(State#state.removeDstBtn),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
              gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},destination}}}),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
              io:format("Destinations Map : ~p~n",[NewDstMap]),
              {noreply, NewState};
            true -> {noreply,State}
          end
      end
  end;

%%event for selecting a node\root from Q2
handle_event(#wx{obj = Q2_Node_List, event = #wxCommand{type = command_listbox_selected}},
    State = #state{node_list_q_2 = Q2_Node_List,msgState = MsgState,destinations = DstMap,destinationCombobox = DestinationComboBox,srcTextBox = SrcTextBox}) ->
  case MsgState of
    notStarted ->
      NewSrc = wxListBox:getStringSelection(Q2_Node_List),
      PidString = queueKeyToPid(NewSrc,0),
      [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
      [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
      gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},src}}}),
      NewMsgState = srcSelected,
      SrcText = "Source: " ++ NewSrc,
      wxTextCtrl:changeValue(SrcTextBox,SrcText),
      %wxStaticText:setLabel(SrcTextBox,"Source: " ++ pid_to_string(Pid)),
      wxButton:enable(State#state.removeSrcBtn),
      NewState = State#state{src = Pid,msgState = NewMsgState},
      {noreply, NewState};
    srcSelected ->
      NewDst = wxListBox:getStringSelection(Q2_Node_List),
      if
        State#state.src == NewDst ->
          io:format("wrong choice mate ~n"),
          {noreply,State};
        true ->
          %%TODO: change the key that is returned to an actual PID in the real version
          PidString = queueKeyToPid(NewDst,0),
          [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
          wxButton:enable(State#state.sendMsg),
          wxButton:enable(State#state.removeDstBtn),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
              gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},destination}}}),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
              io:format("Destinations Map : ~p~n",[NewDstMap]),
              {noreply, NewState};
            true -> {noreply,State}
          end
      end
  end;

%%event for selecting a node\root from Q3
handle_event(#wx{obj = Q3_Node_List, event = #wxCommand{type = command_listbox_selected}},
    State = #state{node_list_q_3 = Q3_Node_List,msgState = MsgState,destinations = DstMap,destinationCombobox = DestinationComboBox,srcTextBox = SrcTextBox}) ->
  case MsgState of
    notStarted ->
      NewSrc = wxListBox:getStringSelection(Q3_Node_List),
      PidString = queueKeyToPid(NewSrc,0),
      [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
      [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
      gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},src}}}),
      NewMsgState = srcSelected,
      SrcText = "Source: " ++ NewSrc,
      wxTextCtrl:changeValue(SrcTextBox,SrcText),
      % wxStaticText:setLabel(SrcTextBox,"Source: " ++ pid_to_string(Pid)),
      wxButton:enable(State#state.removeSrcBtn),
      NewState = State#state{src = Pid,msgState = NewMsgState},
      {noreply, NewState};
    srcSelected ->
      NewDst = wxListBox:getStringSelection(Q3_Node_List),
      if
        State#state.src == NewDst ->
          io:format("wrong choice mate ~n"),
          {noreply,State};
        true ->
          %%TODO: change the key that is returned to an actual PID in the real version
          PidString = queueKeyToPid(NewDst,0),
          [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
          wxButton:enable(State#state.removeDstBtn),
          wxButton:enable(State#state.sendMsg),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
              gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},destination}}}),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
              io:format("Destinations Map : ~p~n",[NewDstMap]),
              {noreply, NewState};
            true -> {noreply,State}
          end
      end
  end;

%%event for selecting a node\root from Q4
handle_event(#wx{obj = Q4_Node_List, event = #wxCommand{type = command_listbox_selected}},
    State = #state{node_list_q_4 = Q4_Node_List,msgState = MsgState,destinations = DstMap,destinationCombobox = DestinationComboBox,srcTextBox = SrcTextBox}) ->
  case MsgState of
    notStarted ->
      NewSrc = wxListBox:getStringSelection(Q4_Node_List),
      PidString = queueKeyToPid(NewSrc,0),
      [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
      [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
      gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},src}}}),
      NewMsgState = srcSelected,
      SrcText = "Source: " ++ NewSrc,
      wxTextCtrl:changeValue(SrcTextBox,SrcText),
      %wxStaticText:setLabel(SrcTextBox,"Source: " ++ pid_to_string(Pid)),
      wxButton:enable(State#state.removeSrcBtn),
      NewState = State#state{src = Pid,msgState = NewMsgState},
      {noreply, NewState};
    srcSelected ->
      NewDst = wxListBox:getStringSelection(Q4_Node_List),
      if
        State#state.src == NewDst ->
          io:format("wrong choice mate ~n"),
          {noreply,State};
        true ->
          %%TODO: change the key that is returned to an actual PID in the real version
          PidString = queueKeyToPid(NewDst,0),
          [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
          wxButton:enable(State#state.removeDstBtn),
          wxButton:enable(State#state.sendMsg),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
              gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},destination}}}),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
              io:format("Destinations Map : ~p~n",[NewDstMap]),
              {noreply, NewState};
            true -> {noreply,State}
          end
      end
  end;

%% mouse click to pick destination - master sends drone
handle_event(#wx{event=#wxMouse{type = left_down}=_Vec}, State = #state{}) -> {noreply, State};

%% send Message event
handle_event(#wx{obj = SendMsgBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{sendMsg = SendMsgBtn,src = Src,destinations = Destinations,msg = MsgTextBox,
      storing_checkbox = StoringCheckBox,msgID = MsgId}) ->
  Msg = wxTextCtrl:getLabel(MsgTextBox),
  DestinationsList = maps:to_list(Destinations),
  OutputList = [Y || {_X,Y} <- DestinationsList],
  io:format("destination ~p~n",[OutputList]),
  comCheck(StoringCheckBox),
  gen_server:call({global,?M_NODE},checkConnections),
  case length(OutputList) of
    0 -> {noreply,State};
    1 ->
      gen_server:cast({global,rplServer},{sendUnicastMessage,Src,hd(OutputList),Msg}), %{sendUnicastMessage, From, To, Msg}
      {noreply,State#state{msgID = MsgId + 1,msgState = sentMsg}};
    _ ->
      io:format("entered multicast option list = ~p~n",[OutputList]),
      MulticastMessageFormat = makeMulticast(OutputList,Src,Msg,MsgId),
      io:format("make multicast list : ~p~n",[MulticastMessageFormat]),
      gen_server:cast({global,rplServer},{sendMulticastMessage,MulticastMessageFormat}), %%send multicast
      {noreply,State#state{msgID = MsgId + length(OutputList),msgState = sentMsg}}
  end;

%% remove dst node
handle_event(#wx{obj = RemoveDst, event = #wxCommand{type = command_button_clicked}},
    State = #state{destinationCombobox = DestinationComboBox,removeDstBtn = RemoveDst,destinations = Destinations}) ->
  NodeToRemove = wxListBox:getStringSelection(DestinationComboBox),
  PidString = queueKeyToPid(NodeToRemove,0),
  [{_,Pid}] = gen_server:call({global,?etsServer},{lookup,?pidStringEts,PidString}),
  [{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
  gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{_Ref,_NumOfRoots,DontCare,_Func,_Type,_Direction,{X,Y},normal}}}),
  NewDestinations = maps:remove(NodeToRemove,Destinations),
  removeNodeFromListBox(NodeToRemove,DestinationComboBox),
  case wxListBox:isEmpty(DestinationComboBox) of
    false -> wxButton:enable(RemoveDst);
    true  -> wxButton:disable(RemoveDst), wxButton:disable(State#state.sendMsg)
  end,
  NewState = State#state{destinations = NewDestinations},
  {noreply,NewState};

%% remove Src node
handle_event(#wx{obj = RemoveSrc, event = #wxCommand{type = command_button_clicked}},
    State = #state{frame = _Frame,removeSrcBtn = RemoveSrc,srcTextBox = SrcTextBox,
      destinationCombobox = DestinationComboBox,removeDstBtn = RemoveDst,src = SrcPid,panel = Panel}) ->
  wxWindow:refresh(Panel),
  [{Pid,{Ref,NumOfRoots,DontCare,Func,Type,Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,SrcPid}),
  gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{Ref,NumOfRoots,DontCare,Func,Type,Direction,{X,Y},normal}}}),
  wxTextCtrl:changeValue(SrcTextBox,"Source : null"),
  %wxStaticText:setLabel(SrcTextBox,"Source: null"),
  PidList = gen_server:call({global,?etsServer},{getAll,?locationEts}),
  NewPidList = [{_Pid,{_Ref,_NumOfRoots,_DontCare,_Func,_Type,_Direction,{_X,_Y},normal}} || {_Pid,{_Ref,_NumOfRoots,_DontCare,_Func,_Type,_Direction,{_X,_Y},_State}} <- PidList],
%% todo: check if inserting multiple elements is done correctly
  gen_server:call({global,?etsServer},{insert,?locationEts,NewPidList}),
  resetQueue(DestinationComboBox,wxListBox:isEmpty(DestinationComboBox)),
  wxButton:disable(RemoveSrc),
  wxButton:disable(RemoveDst),
  wxButton:disable(State#state.sendMsg),
  NewState = State#state{src = nullptr,destinations = #{},msgState = notStarted,srcTextBox = SrcTextBox},
  {noreply,NewState};

handle_event(#wx{event = #wxClose{}},State = #state{panel = Panel}) ->
%handle_event(#wx{event=#wxClose{type=close_window}},State = #state{frame = Frame,panel = Panel}) ->
  wxPanel:destroy(Panel),
  io:format("Exiting~n"),
  exit(self(),quit),
  {stop,normal,State};

handle_event(#wx{obj = QuitBtn, event = #wxClose{type = command_button_clicked}},
    State = #state{quit = QuitBtn,frame = Frame,panel = Panel}) ->
  exit(self(),quit);


%%create root event
handle_event(#wx{obj = NewRootBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{frame = _Frame,newRootBtn = NewRootBtn,panel = Panel}) ->
  {Result,StateA} = create(root,State),
  if
    Result == error -> {noreply,State} ;
    true ->
      %{_DrawStatus,StateB} = draw(StateA),
      wxWindow:refresh(Panel),
      %wxPanel:refresh(Panel),
      {noreply,StateA}
  end;
%%create node event
handle_event(#wx{obj = NewNodeBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{newNodeBtn = NewNodeBtn,panel = Panel}) ->
  {Result,_StateA} = create(node,State),
  if
    Result == error -> {noreply,State} ;
    true ->
      wxPanel:refresh(Panel),
      {noreply,_StateA}
  end.

handle_call(test, _From, State = #state{}) ->
  %io:format("got draw ~n"),
  Reply = 1,
  {reply,Reply,State};

handle_call({draw}, _From, State = #state{panel = Panel}) ->
  wxPanel:refresh(Panel),
  %io:format("got draw ~n"),
  Reply = ok,
  {reply,Reply,State};

handle_call(Request, _From, State = #state{}) ->
  {reply,Request,State}.

%% handle event for sending a message from the RPL server - when there is an empty path
handle_cast({messageSent,_MsgId,[]}, State = #state{}) ->
  %%insert to queue of unsent messages
  NewState = State#state{msgState = notStarted},
  {noreply,NewState};

%% handle event for sending a message from the RPL server.
handle_cast({messageSent,MsgId,Pathlist}, State = #state{destinationCombobox = DestinationQeueu,srcTextBox = SrcTextBox}) ->
  Path = makePath(Pathlist),
  io:format("path is: ~p~n",[Path]),
  ets:insert(?pathEts,{MsgId,Path}),
  PidList = gen_server:call({global,?etsServer},{getAll,?locationEts}),
  NewPidList = [{_Pid,{_Ref,_NumOfRoots,_DontCare,_Func,_Type,_Direction,{_X,_Y},normal}} || {_Pid,{_Ref,_NumOfRoots,_DontCare,_Func,_Type,_Direction,{_X,_Y},_State}} <- PidList],
%% todo: check if inserting multiple elements is done correctly
  gen_server:call({global,?etsServer},{insert,?locationEts,NewPidList}),
  resetQueue(DestinationQeueu,wxListBox:isEmpty(DestinationQeueu)),
  wxTextCtrl:changeValue(SrcTextBox,"Source : null"),
  NewState = State#state{msgState = notStarted,src = nullptr},
  {noreply,NewState};

handle_cast(draw, State = #state{panel = Panel}) ->
  %{_Reply,NewState} = wxPanel:refresh(Panel),
  wxPanel:refresh(Panel),
  %{noreply,NewState};
  {noreply,State};

handle_cast(_Msg, State) ->
  {noreply,State}.

% Handle falls from root or Node Server
handle_info({'DOWN', Ref, process, Pid, {rplCrash, Reason, ProcState}}, State= #state{}) ->
  io:format("rplServer Monitor crash, Ref: ~p , Pid: ~p, Reason: ~p State: ~p~n", [Ref, Pid, Reason, ProcState]),
  case utils:getCorrectNodeToSpawn(gfx) of
    ?R_NODE -> rpc:call('r_node@amirs-MacBook-Pro',rootWrapper,startRPL,[ProcState]);
    ?G_NODE -> rpc:call('g_node@amirs-MacBook-Pro',rplWrapper,startRPL,[ProcState]);
    ?N_NODE -> rpc:call('n_node@amirs-MacBook-Pro',nodeWrapper,startRPL,[ProcState]);
    ?M_NODE -> rplServer:start_link(ProcState)
  end,
  io:format("RPL crashed, restarted RPL server~n"),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.
code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(Reason, State) ->
  %whereis(?APP_SERVER)!{gfxTerminate,self(),_Reason},
  io:format("entered terminate ~n"),
  wx:destroy(),
  exit({gfxCrash, Reason, State}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makePath([]) -> [];
makePath(Path) when length(Path) == 2 -> {hd(Path),hd(tl(Path))};
makePath([Head|T]) -> [{Head,hd(T)} | makePath(T)].


makeMulticast([H|[]],Src,Msg,MsgId) -> [{messageFormat,MsgId,Src,H,Msg}];
makeMulticast([H|T],Src,Msg,MsgId) ->
  io:format("makeMulticast([H|T],Src,Msg):: H = ~p T = ~p~n",[H,T]),
  [{messageFormat,MsgId,Src,H,Msg}] ++ makeMulticast(T,Src,Msg,MsgId + 1).

removeNodeFromListBox(Node,ListBox) ->
  DstList = listBoxToList(ListBox,false),
  NewList = [X || X <- DstList, X =/= Node],
  lists:foreach(fun(X) -> wxListBox:append(ListBox,X) end,NewList).

listBoxToList(_ListBox,true) -> [];
listBoxToList(ListBox,false) ->
  Node = wxListBox:getString(ListBox,0),
  wxControlWithItems:delete(ListBox,0),
  [Node | listBoxToList(ListBox,wxListBox:isEmpty(ListBox))].

queueKeyToPid(Key,0) ->
  queueKeyToPid(tl(Key),1);
queueKeyToPid(Key,1) ->
  queueKeyToPid(tl(Key),2);
queueKeyToPid(Key,2) -> Key.

integer_to_string(Integer) when is_integer(Integer) ->
  erlang:integer_to_list(Integer).
%lists:flatten(io_lib:format("~p", [Integer])).

init_layout(Mode,Node) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx,-1,"RPL Simulation",[{size,{?MapSize + 600,?MapSize + 50}}]),
  Panel = wxPanel:new(Frame,[{size,{?MapSize,?MapSize}},{style,?wxFULL_REPAINT_ON_RESIZE}]),
  %io:format("color is ~p~n",[?wxBLUE]),
  wxPanel:setBackgroundColour(Panel,?wxSheikBlue),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ControlSizer = wxBoxSizer:new(?wxVERTICAL),
  %SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL}],
  %% create all UI elements %%
  NewRootBtn = wxButton:new(Frame,?wxID_ANY,[{label,"Create Root"}]),
  NewNodeBtn = wxButton:new(Frame,?wxID_ANY,[{label,"Create Node"}]),
  QuitBtn    = wxButton:new(Frame,?wxID_ANY,[{label,"Quit"}]),
  SendMsgBtn = wxButton:new(Frame,?wxID_ANY,[{label,"Send Message"}]),
  StartBtn   = wxButton:new(Frame,?wxID_ANY,[{label,"Start"}]),
  RemoveSrc  = wxButton:new(Frame,?wxID_ANY,[{label,"Remove source"}]),
  RemoveDst  = wxButton:new(Frame,?wxID_ANY,[{label,"Remove desination"}]),
  DestinationList = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Storing_checkBox = wxCheckBox:new(Frame,?wxID_ANY,"Storing",[]),
  NStoring_checkBox = wxCheckBox:new(Frame,?wxID_ANY,"None Storing",[]),
  %SrcTxt = wxStaticText:new(Frame,?wxID_ANY,"Source : null",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  SrcTxt = wxTextCtrl:new(Frame,1,[{value, "Source : null"}, {style, ?wxDEFAULT}]),
  List_NQ_1_Label = wxStaticText:new(Frame,?wxID_ANY,"     0",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_NQ_2_Label = wxStaticText:new(Frame,?wxID_ANY,"     0",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_NQ_3_Label = wxStaticText:new(Frame,?wxID_ANY,"     0",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_NQ_4_Label = wxStaticText:new(Frame,?wxID_ANY,"     0",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),


  Node_list_q_1 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_2 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_3 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_4 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  MessageTextBox = wxTextCtrl:new(Frame, ?wxID_ANY, [{size, {100,100}},{value, "Write message here ..."}, {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
  MovementLabel = wxStaticText:new(Frame,?wxID_ANY,"Movement type",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  wxStaticText:wrap(MovementLabel,-1),
  Choices = ["","Random","Polynomial","Sinusoidal"],
  MovementChooser = wxComboBox:new(Frame, length(Choices), [{choices, Choices}]),
  wxComboBox:setToolTip(MovementChooser, "Movement Type"),

  %% order UI elements in sizers %%
  %% create UI sizers %%
  wxWindow:setSizer(Frame, MainSizer),
  %wxSizer:setSizeHints(MainSizer,Frame),
  %wxWindow:setMinSize(Frame,wxWindow:getSize(Frame)),
  CreateSizer = wxBoxSizer:new(?wxVERTICAL),
  Q1Sizer = wxBoxSizer:new(?wxVERTICAL),
  Q2Sizer = wxBoxSizer:new(?wxVERTICAL),
  Q3Sizer = wxBoxSizer:new(?wxVERTICAL),
  Q4Sizer = wxBoxSizer:new(?wxVERTICAL),
  _QueueSizer = wxBoxSizer:new(?wxHORIZONTAL),
  MessageSizer_1 = wxBoxSizer:new(?wxVERTICAL),
  MessageSizer_2 = wxBoxSizer:new(?wxHORIZONTAL),
  StartEndSizer = wxBoxSizer:new(?wxVERTICAL),
  CheckBoxSizer = wxBoxSizer:new(?wxHORIZONTAL),
  MessageSizer = wxBoxSizer:new(?wxVERTICAL),
  DestinationListBox = wxStaticBoxSizer:new(?wxVERTICAL,Frame,[{label, "Destinations"}]),
  _List_Q_1_Label = wxStaticBoxSizer:new(?wxVERTICAL,Frame,[{label, "Q1"}]),
  _List_Q_2_Label = wxStaticBoxSizer:new(?wxVERTICAL,Frame,[{label, "Q2"}]),
  _List_Q_3_Label = wxStaticBoxSizer:new(?wxVERTICAL,Frame,[{label, "Q3"}]),
  _List_Q_4_Label = wxStaticBoxSizer:new(?wxVERTICAL,Frame,[{label, "Q4"}]),

  wxSizer:add(CheckBoxSizer,Storing_checkBox,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CheckBoxSizer,NStoring_checkBox,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(StartEndSizer,StartBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(StartEndSizer,CheckBoxSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(StartEndSizer,QuitBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(MessageSizer_1,SrcTxt,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(MessageSizer_1,RemoveSrc,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(MessageSizer_1,RemoveDst,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(MessageSizer_2,MessageSizer_1,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(DestinationListBox,DestinationList,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(MessageSizer_2,DestinationListBox,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q1Sizer,Node_list_q_1,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q1Sizer,List_NQ_1_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(_List_Q_1_Label,Q1Sizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q2Sizer,Node_list_q_2,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q2Sizer,List_NQ_2_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(_List_Q_2_Label,Q2Sizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q3Sizer,Node_list_q_3,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q3Sizer,List_NQ_3_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(_List_Q_3_Label,Q3Sizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q4Sizer,Node_list_q_4,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(Q4Sizer,List_NQ_4_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(_List_Q_4_Label,Q4Sizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(_QueueSizer,_List_Q_1_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(_QueueSizer,_List_Q_2_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(_QueueSizer,_List_Q_3_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(_QueueSizer,_List_Q_4_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(CreateSizer,MovementLabel,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,MovementChooser,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,NewRootBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,NewNodeBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MessageSizer,MessageTextBox,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MessageSizer,SendMsgBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MessageSizer,MessageSizer_2,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),

  wxBoxSizer:add(ControlSizer,CreateSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,MessageSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,_QueueSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,StartEndSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MainSizer,ControlSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MainSizer,Panel,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),

  %% connect all elements %%
  wxFrame:connect(Frame, close_window, [{skip, true}]),
  %wxFrame:connect(Frame,close_window),
  wxButton:connect(StartBtn,command_button_clicked),
  wxButton:connect(RemoveDst,command_button_clicked),
  wxButton:connect(RemoveSrc,command_button_clicked),
  wxCheckBox:connect(Storing_checkBox, command_checkbox_clicked),
  wxCheckBox:connect(NStoring_checkBox, command_checkbox_clicked),
  wxButton:connect(NewRootBtn,command_button_clicked),
  wxButton:connect(NewNodeBtn,command_button_clicked),
  wxButton:connect(SendMsgBtn,command_button_clicked),
  wxListBox:connect(Node_list_q_1,command_listbox_selected),
  wxListBox:connect(Node_list_q_2,command_listbox_selected),
  wxListBox:connect(Node_list_q_3,command_listbox_selected),
  wxListBox:connect(Node_list_q_4,command_listbox_selected),
  wxComboBox:connect(MovementChooser, command_combobox_selected),
  wxButton:connect(QuitBtn,command_button_clicked),
  %% config ui for start of run %%
  wxButton:disable(SendMsgBtn),
  wxButton:disable(NewNodeBtn),
  wxButton:disable(NewRootBtn),
  wxButton:disable(RemoveSrc),
  wxButton:disable(RemoveDst),
  wxButton:disable(StartBtn),
  wxButton:disable(SendMsgBtn),
  %% show frame %%
  wxPanel:connect(Panel, left_down),
  wxPanel:connect(Panel, paint, [callback]),
  wxFrame:show(Frame),
  %io:format("~p~n",[wxPanel:getSize(Panel)]),
  #state
  {
    frame = Frame, panel = Panel, newRootBtn = NewRootBtn, newNodeBtn = NewNodeBtn, quit = QuitBtn, sendMsg = SendMsgBtn,
    node_list_q_1 = Node_list_q_1, node_list_q_2 = Node_list_q_2, node_list_q_3 = Node_list_q_3,node_list_q_4 = Node_list_q_4,
    movementList = MovementChooser, moveType = "", nodeTypetoCreate = "", size = ?MapSize, protocolServer = "",
    appState = initiated, msgTextBox = Frame, mode = Mode, node = Node, msg = MessageTextBox,locationMap = #{},
    numOfNodes = 0, numOfRoots = 0, nq1 = List_NQ_1_Label,nq2 = List_NQ_2_Label,nq3 = List_NQ_3_Label,nq4 =  List_NQ_4_Label,
    msgState = notStarted,src = nullptr, destinations = #{},startBtn = StartBtn,removeDstBtn = RemoveDst,removeSrcBtn = RemoveSrc,
    storing_checkbox = Storing_checkBox,nonStoring_checkbox = NStoring_checkBox,destinationCombobox = DestinationList,
    srcTextBox = SrcTxt, msgID = 0, pathList = #{},createdFirst = false,debug = 350
  }.

create(RootOrNode, State = #state{numOfRoots = NumOfRoots,
  numOfNodes = NumOfNodes,createdFirst = CreatedFirst,storing_checkbox = StoringCheckBox})  ->
  {Func,Type} = case State#state.moveType of
                  "Random" -> {random,random};
                  "Polynomial" -> {funcGenerator:generatePolynom(1,[]),polynomial};
                  "Sinusoidal" -> {funcGenerator:generateSin(rand:uniform(300),[]),sinusoidal};
                  _ -> badArguement
                end,
  X = rand:uniform(?MapSize),
  {FinalX,FinalY} = case Type of
                      random     -> {rand:uniform(?MapSize),rand:uniform(?MapSize)};
                      polynomial -> getStartingPos(Func,Type,X,RootOrNode, State);
                      sinusoidal -> getStartingPos(Func,Type,X,RootOrNode, State)
                    end,
  comCheck(StoringCheckBox),
  Midstate = case RootOrNode of
               root ->
                 %{Pid,Ref} = gen_server:call(?APP_SERVER, {addNode, root}),
                 {Pid,Ref} = gen_server:call({global,rplServer},{addNode,root}),
                 %{Pid,Ref} = rpc:call('g_node@amirs-MacBook-Pro',rplWrapper,rpcReq,[{addNode, root}]),
                 gen_server:call({global,?etsServer},{insert,?nodePidsEts,{nodePid,Pid}}),
                 gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{Ref,NumOfRoots,RootOrNode,Func,Type,?incerement,{FinalX,FinalY},normal}}}),
                 gen_server:call({global,?etsServer},{insert,?ROOT_LIST,{Pid, {Ref,FinalX,FinalY}}}),
                 State#state{numOfRoots = NumOfRoots + 1};
               node ->
                 {Pid1,Ref1} = gen_server:call({global,rplServer},{addNode,node}),
                 gen_server:call({global,?etsServer},{insert,?nodePidsEts,{nodePid,Pid1}}),
                 gen_server:call({global,?etsServer},{insert,?locationEts,{Pid1,{Ref1,NumOfRoots,RootOrNode,Func,Type,?incerement,{FinalX,FinalY},normal}}}),
                 State#state{numOfNodes = NumOfNodes + 1}
             end,
  if
    CreatedFirst == false -> gen_server:cast({global,?M_NODE},drawGFX);
    true -> io:format("Created a new node,create flag= ~p~n",[CreatedFirst])
  end,
  FinalState = Midstate#state{createdFirst = true},
  if
    RootOrNode == root -> {root_OK,FinalState};
    true -> {node_OK,FinalState}
  end.

getStartingPos(Func,Type,X,_RootOrNode, _State) ->
  {_NewX,Yt} = case Type of
                 polynomial -> funcGenerator:solveP(Func,transformXtoT(X),0);
                 sinusoidal -> funcGenerator:solveS(Func,transformTtoX(X))
               end,
  Y = transformTtoX(Yt),
  if
    Y > ?MapSize orelse Y < 0 ->
      % io:format("failed coordinates"),
      {0,0};
    true -> {X,erlang:floor(Y)}
  end.


draw(State = #state{panel = Panel,node_list_q_1 = Q1,node_list_q_2 = Q2,
  node_list_q_3 = Q3,node_list_q_4 = Q4}) ->
  LocationList = gen_server:call({global,?etsServer},{getAll,?locationEts}),
  %PidList = gen_server:call({global,?etsServer},{lookup,?nodePidsEts,nodePid}),
  eraseAllOldLocs(Panel),
  drawGrid(Panel),
  resetQueus(Q1,Q2,Q3,Q4),
  {Nq1,Nq2,Nq3,Nq4} = update(Panel,LocationList,Q1,Q2,Q3,Q4),
  drawPaths(Panel),
  LocUpdateState = State#state{node_list_q_1 = Nq1,node_list_q_2 = Nq2,node_list_q_3 = Nq3,node_list_q_4 = Nq4},
  NewState =  updateQueues(LocUpdateState),
  {draw_OK,NewState}.

drawPaths(Panel) ->
  PathsList = ets:tab2list(?pathEts),
  lists:foreach(fun(Path) -> drawPath(erlang:element(2,Path),Panel) end, PathsList),
  ets:delete_all_objects(?pathEts).


removeKeys([H|[]],PathMap) ->
  maps:remove(H,PathMap);
removeKeys(Keylist,PathMap) ->
  NewMap = maps:remove(hd(Keylist),PathMap),
  removeKeys(tl(Keylist),NewMap).



drawPath([],_Panel) -> ok;
drawPath([H|[]],Panel) ->
  [{_,{_,_,_,_,_,_,{Xs,Ys},_}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,erlang:element(1,hd(H))}),
  [{_,{_,_,_,_,_,_,{Xd,Yd},_}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,erlang:element(2,hd(H))}),
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxRED),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawLine(Paint,{Xs,Ys},{Xd,Yd}),
  wxClientDC:destroy(Paint);
drawPath(PathList,Panel) when not(is_tuple(PathList)) ->
  [{_,{_,_,_,_,_,_,{Xs,Ys},_}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,erlang:element(1,hd(PathList))}),
  [{_,{_,_,_,_,_,_,{Xd,Yd},_}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,erlang:element(2,hd(PathList))}),
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxRED),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawLine(Paint,{Xs,Ys},{Xd,Yd}),
  wxClientDC:destroy(Paint),
  drawPath(tl(PathList),Panel);
drawPath(PathList,Panel) when is_tuple(PathList) ->
  [{_,{_,_,_,_,_,_,{Xs,Ys},_}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,erlang:element(1,PathList)}),
  [{_,{_,_,_,_,_,_,{Xd,Yd},_}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,erlang:element(2,PathList)}),
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxRED),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawLine(Paint,{Xs,Ys},{Xd,Yd}),
  wxClientDC:destroy(Paint).

update(_Panel,[],Q1,Q2,Q3,Q4) -> {Q1,Q2,Q3,Q4};
update(Panel,[{Pid,{_Ref,_NumOfRoots,NodeType,_Func,_Type,_Direction,{X,Y},MsgRole}}|[]],Q1,Q2,Q3,Q4) ->
  %[{Pid,{_Ref,_NumOfRoots,NodeType,_Func,_Type,_Direction,{X,Y},MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
  {NewX,NewY} = updateLocation({Pid,{_Ref,_NumOfRoots,NodeType,_Func,_Type,_Direction,{X,Y},MsgRole}}),
  {Nq1,Nq2,Nq3,Nq4} = insertToQueues(Pid,{NewX,NewY},NodeType,Q1,Q2,Q3,Q4),
  drawNode({NewX,NewY},NodeType,MsgRole,Panel),
  %drawNode({NewX,NewY},NodeType,MsgRole,Panel),
  {Nq1,Nq2,Nq3,Nq4};
update(Panel,[{Pid,{_Ref,_NumOfRoots,NodeType,_Func,_Type,_Direction,{X,Y},MsgRole}}|T],Q1,Q2,Q3,Q4) ->
  %[{Pid,{_Ref,_NumOfRoots,NodeType,_Func,_Type,_Direction,{X,Y},MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
  {NewX,NewY} = updateLocation({Pid,{_Ref,_NumOfRoots,NodeType,_Func,_Type,_Direction,{X,Y},MsgRole}}),
  {Nq1,Nq2,Nq3,Nq4} = insertToQueues(Pid,{NewX,NewY},NodeType,Q1,Q2,Q3,Q4),
  drawNode({NewX,NewY},NodeType,MsgRole,Panel),
  %drawNode({NewX,NewY},NodeType,MsgRole,Panel),
  update(Panel,T,Nq1,Nq2,Nq3,Nq4).


updateLocation({Pid,{Ref,NumOfRoots,_NodeType,Func,MovementType,Direction,{X,Y},MsgRole}}) ->
  {NewX,NewY,NewDirection} = case MovementType of
                               random -> getRandomCoordinates(X,Y);
                               polynomial -> getPolynomialCoordinates(Func,Direction,X);
                               sinusoidal -> getSinusoidalCoordinates(Func,Direction,X)
                             end,
  gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{Ref,NumOfRoots,_NodeType,Func,MovementType,NewDirection,{NewX,NewY},MsgRole}}}),
  {NewX,NewY}.

drawGrid(Panel) ->
  Paint = wxPaintDC:new(Panel),
  Brush = wxBrush:new(?wxBLACK,[{style,?wxTRANSPARENT}]),
  wxDC:setBrush(Paint,Brush),
  wxDC:drawLine(Paint,{0,?GridMapSize},{?MapSize,?GridMapSize}),
  wxDC:drawLine(Paint,{?GridMapSize,0},{?GridMapSize,?MapSize}),
  wxPaintDC:destroy(Paint).

resetQueue(_,true) -> {ok};
resetQueue(Q_n,false) ->
  wxControlWithItems:delete(Q_n,0),
  Flag = wxListBox:isEmpty(Q_n),
  resetQueue(Q_n,Flag).

resetQueus(Q1,Q2,Q3,Q4) ->
  resetQueue(Q1,wxListBox:isEmpty(Q1)),
  resetQueue(Q2,wxListBox:isEmpty(Q2)),
  resetQueue(Q3,wxListBox:isEmpty(Q3)),
  resetQueue(Q4,wxListBox:isEmpty(Q4)).

updateQueues(State = #state{node_list_q_4 = Q4,node_list_q_3 = Q3,node_list_q_2 = Q2,node_list_q_1 = Q1,nq1 = Nq1,nq2 = Nq2,nq3 = Nq3,nq4 = Nq4}) ->
  wxStaticText:setLabel(Nq1,integer_to_string(wxListBox:getCount(Q1))),
  wxStaticText:setLabel(Nq2,integer_to_string(wxListBox:getCount(Q2))),
  wxStaticText:setLabel(Nq3,integer_to_string(wxListBox:getCount(Q3))),
  wxStaticText:setLabel(Nq4,integer_to_string(wxListBox:getCount(Q4))),
  State.

insertToQueues(Pid,{X,Y},NodeType,Q1,Q2,Q3,Q4) ->
  AtomString = case NodeType of
                 root -> "r_";
                 node -> "n_"
               end,
  PidString = pid_to_string(Pid),
  A = lists:flatten(io_lib:format("~p", [hd(PidString)])),
  B = lists:flatten(io_lib:format("~p", [hd(tl(PidString))])),
  C = lists:flatten(io_lib:format("~p", [hd(tl(tl(PidString)))])),
  ListKey = AtomString ++ A ++ B ++ C,
  gen_server:call({global,?etsServer},{insert,?pidStringEts,{A ++ B ++ C , Pid}}),
  case {X,Y}  of
    {X1,Y1} when X1 >= ?GridMapSize andalso Y1 >= ?GridMapSize ->
      wxListBox:append(Q1,[ListKey]);
    {X2,Y2} when X2 < ?GridMapSize andalso Y2 > ?GridMapSize ->
      wxListBox:append(Q2,[ListKey]);
    {X3,Y3} when X3 =< ?GridMapSize andalso Y3 =< ?GridMapSize ->
      wxListBox:append(Q3,[ListKey]);
    _ ->
      wxListBox:append(Q4,[ListKey])
  end,
  {Q1,Q2,Q3,Q4}.


pid_to_string(Pid) when is_pid(Pid) ->
  PidStr = pid_to_list(Pid),
  PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
  [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])];
% {N, P1, P2};
pid_to_string(Pid) when is_integer(Pid) -> integer_to_string(Pid).
%updateAllCoordinates([H|[]],Panel) ->
% io:format("enter updateCoordinate: ~p ~n",[H]),
%updateCoordinates(H);
%updateAllCoordinates(PidList,Panel) ->
%  io:format("enter updateCoordinate: ~p~n",[hd(PidList)]),
% updateCoordinates(hd(PidList)),
%updateAllCoordinates(tl(PidList)).



eraseAllOldLocs(Panel) -> eraseNodes(Panel).

drawNode({X,Y},root,src,Panel) ->
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxYellow),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  BrushE = wxBrush:new(?wxSheikBlue),
  wxClientDC:setBrush(Paint,BrushE),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius div 2),  %draw circle center at {X,Y}
  wxClientDC:destroy(Paint);

drawNode({X,Y},root,destination,Panel) ->
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxOrange),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  BrushE = wxBrush:new(?wxSheikBlue),
  wxClientDC:setBrush(Paint,BrushE),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius div 2),  %draw circle center at {X,Y}
  wxClientDC:destroy(Paint);

drawNode({X,Y},root,normal,Panel) ->
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxRED),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  BrushE = wxBrush:new(?wxSheikBlue),
  wxClientDC:setBrush(Paint,BrushE),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius - 2),  %draw circle center at {X,Y}
  wxClientDC:destroy(Paint);

drawNode({X,Y},node,src,Panel) ->
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxDarkGreen),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  BrushE = wxBrush:new(?wxSheikBlue),
  wxClientDC:setBrush(Paint,BrushE),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius div 2),  %draw circle center at {X,Y}
  wxClientDC:destroy(Paint);

drawNode({X,Y},node,destination,Panel) ->
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxDarkCyan),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  BrushE = wxBrush:new(?wxSheikBlue),
  wxClientDC:setBrush(Paint,BrushE),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius div 2),  %draw circle center at {X,Y}
  wxClientDC:destroy(Paint);

drawNode({X,Y},node,normal,Panel) ->
  Paint = wxClientDC:new(Panel),
  Brush = wxBrush:new(?wxBLACK),
  wxClientDC:setBrush(Paint,Brush),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  BrushE = wxBrush:new(?wxSheikBlue),
  wxClientDC:setBrush(Paint,BrushE),
  wxClientDC:drawCircle(Paint ,{X,Y},?radius - 2),  %draw circle center at {X,Y}
  wxClientDC:destroy(Paint).



eraseNodes(Panel) ->
  Paint = wxPaintDC:new(Panel),
  Brush = wxBrush:new(?wxSheikBlue),
  wxDC:setBrush(Paint,Brush),
  wxDC:drawRectangle(Paint,{0,0},{?MapSize,?MapSize}),
  wxPaintDC:destroy(Paint).


%updateCoordinates({Pid,{Ref,NumOfRoots,_DontCare,Func,MovementType,Direction,{X,Y}}}) ->
updateCoordinates({nodePid,Pid}) ->
  [{Pid,{Ref,NumOfRoots,RootOrNode,Func,MovementType,Direction,{X,Y},_MsgRole}}] = gen_server:call({global,?etsServer},{lookup,?locationEts,Pid}),
  {NewX,NewY,NewDirection} = case MovementType of
                               random -> getRandomCoordinates(X,Y);
                               polynomial -> getPolynomialCoordinates(Func,Direction,X);
                               sinusoidal -> getSinusoidalCoordinates(Func,Direction,X)
                             end,
  if
    RootOrNode == root ->
      gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{Ref,NumOfRoots,RootOrNode,Func,MovementType,NewDirection,{NewX,NewY}}}}),
      gen_server:call({global,?etsServer},{insert,?ROOT_LIST,{Pid,{Ref,NewX,NewY}}});
    true -> gen_server:call({global,?etsServer},{insert,?locationEts,{Pid,{Ref,NumOfRoots,RootOrNode,Func,MovementType,NewDirection,{NewX,NewY}}}})
  end.

getRandomCoordinates(X,Y) ->
  %io:format("before random coordinates ~n"),
  {getRandomCoordinate(X),getRandomCoordinate(Y),?incerement}.

getRandomCoordinate(X) ->
  Delta = rand:uniform(?RandMovement),
  Direction = rand:uniform(2),
  NewDelta = case Direction of
               1 -> Delta;
               2 -> -1 * Delta
             end,
  %io:format("new delta = ~p~n",[NewDelta]),
  %io:format("entered getRandomCoordinate ~p ~p ~n",[Delta,X+Delta]),
  if
    (X + NewDelta > ?MapSize) orelse (X + NewDelta < 0) -> getRandomCoordinate(X);
    true -> X + NewDelta
  end.

getPolynomialCoordinates(Func,Direction,X) ->
  case Direction of
    1 -> if
           X + ?polynomDelta >= ?MapSize -> getPolynomialCoordinates(Func,?decrement,X);
           true ->
             T = transformXtoT(X+?polynomDelta),
             {NewXt,NewYt} = funcGenerator:solveP(Func,T,0),
             {_NewX,NewY} = {transformTtoX(NewXt),transformTtoX(NewYt)},
             if
               (NewY > ?MapSize) orelse (NewY < 0) -> getPolynomialCoordinates(Func, Direction,X + ?polynomDelta);
               true ->
                 %  io:format("T = ~p OldX = ~p NewX = ~p NewY = ~p~n",[T,X,NewX,erlang:floor(NewY)]),
                 {X + ?polynomDelta,erlang:floor(NewY),Direction}
             end
         end;
    -1 -> if
            X - ?polynomDelta < 0 -> getPolynomialCoordinates(Func,?incerement,X);
            true ->
              T = transformXtoT(X+?polynomDelta),
              {NewXt,NewYt} = funcGenerator:solveP(Func,T,0),
              {_NewX,NewY} = {transformTtoX(NewXt),transformTtoX(NewYt)},
              if
                NewY > ?MapSize orelse NewY < 0 -> getPolynomialCoordinates(Func, Direction,X - ?polynomDelta);
                true ->
                  % io:format("T = ~p NewX = ~p NewY = ~p~n",[T, NewX,erlang:floor(NewY)]),
                  {X - ?polynomDelta,erlang:floor(NewY),Direction}
              end
          end
  end.




getSinusoidalCoordinates(Func,Direction,X) ->
  case Direction of
    1 -> if
           X + ?sinDelta >= ?MapSize -> getSinusoidalCoordinates(Func,?decrement,X);
           true ->
             T = transformXtoT(X+?sinDelta),
             {NewXt,NewYt} = funcGenerator:solveS(Func,T),
             {_NewX,NewY} = {transformTtoX(NewXt),transformTtoX(NewYt)},
             if
               (NewY > ?MapSize) orelse (NewY < 0) -> getSinusoidalCoordinates(Func, Direction,X + ?sinDelta);
               true ->
                 %io:format("T = ~p OldX = ~p NewX = ~p NewY = ~p~n",[T,X,NewX,erlang:floor(NewY)]),
                 {X + ?sinDelta,erlang:floor(NewY),Direction}
             end
         end;
    -1 -> if
            X - ?sinDelta < 0 -> getSinusoidalCoordinates(Func,?incerement,X);
            true ->
              T = transformXtoT(X+?sinDelta),
              {NewXt,NewYt} = funcGenerator:solveS(Func,T),
              {_NewX,NewY} = {transformTtoX(NewXt),transformTtoX(NewYt)},
              if
                NewY > ?MapSize orelse NewY < 0 -> getSinusoidalCoordinates(Func, Direction,X - ?sinDelta);
                true ->
                  %io:format("T = ~p NewX = ~p NewY = ~p~n",[T, NewX,erlang:floor(NewY)]),
                  {X - ?sinDelta,erlang:floor(NewY),Direction}
              end
          end
  end.

transformXtoT(X) -> X - 350.
transformTtoX(T) -> T + 350.

comCheck(StoringCheckBox) ->
  io:format("comCheck:: ~p~n",[global:whereis_name(?RPL_SERVER)]),
  case global:whereis_name(?RPL_SERVER) of
     undefined ->
       Flag = wxCheckBox:isChecked(StoringCheckBox),
       if
         Flag == true ->
           Argument = ?STORING;
         true ->
           Argument = ?NON_STORING
       end,
        Reply = case utils:getCorrectNodeToSpawn(gfx) of
                    ?R_NODE ->
                      gen_server:call({global, rootWrapper},{startRPL,Argument}),
                      %rpc:call('g_node@amirs-MacBook-Pro',rootWrapper,startRPL,[Argument]),
                      io:format("comCheck:: ~p~n",[global:whereis_name(rplServer)]);
                    ?G_NODE ->
                      gen_server:call({global, rplWrapper},{startRPL,Argument}),
                      %rpc:call('r_node@amirs-MacBook-Pro',rplWrapper,startRPL,[Argument]),
                        io:format("comCheck:: ~p~n",[global:whereis_name(rplServer)]);
                    ?N_NODE ->
                      gen_server:call({global, nodeWrapper},{startRPL,Argument}),
                      %rpc:call('n_node@amirs-MacBook-Pro',nodeWrapper,startRPL,[Argument]),
                      io:format("comCheck:: ~p~n",[global:whereis_name(rplServer)]);

                    ?M_NODE ->
                     % gen_server:call({global, rootWrapper},{startRPL,Argument}),
                      global:unregister_name(rplServer),
                      rplServer:start_link(Argument),
                      io:format("comCheck:: ~p~n",[global:whereis_name(rplServer)])
        end;
    _ -> Reply = ok
  end,
  Reply.