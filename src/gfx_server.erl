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
-export([start/1, init/1, terminate/2, code_change/3, handle_info/2, handle_call/3, handle_cast/2, handle_event/2,start_global/1]).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

-define(NODE_LIST, nodeList).
-define(ROOT_LIST, rootList).
-define(ImageSizeX, 15).
-define(ImageSizeY, 11).
-define(DEFAULTSPEED, 100).
-define(MapSize, 1000).
-define(DroneSize, 20).
-define(SERVER,?MODULE).
-define(GRAPH_ACC,5).
-define(max_x,(1000)). %1573
-define(max_y,(1000)).
-define(table_name,gfx_ets).
-define(wxSheikBlue,{16#DF,16#FA,16#DE,16#FF}).
-define(RandMovement,5).
-define(polynomDelta,1).
-define(sinDelta,5).
-define(incerement,1).
-define(decrement,-1).
-define(testCords,{500,500}).
-define(radius,50).
-define(transformedMap,1000).
-record(state,{appState,newRootBtn,movementList,nodeTypetoCreate,newNodeBtn,sendMsg,moveType,quit,
  node_list_q_1,node_list_q_2,node_list_q_3,node_list_q_4,nq1,nq2,nq3,nq4,panel,size,frame,
  protocolServer,msgTextBox,mode,node,id,locationMap,msg,numOfNodes,numOfRoots,tableID,msgState,
  src,destinations,srcTextBox,destinationCombobox,startBtn,storing_checkbox,nonStoring_checkbox,
  removeSrcBtn,removeDstBtn,msgID,pathList}).



start_global(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[global,Node],[]).

start(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[local,Node],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Mode,Node]) ->
  %InitState = init_test(Mode,Node),
  InitState = init_layout(Mode,Node),
  io:format("done init ~n"),
  {InitState#state.frame,InitState}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%handle_sync_event(#wx{event=#wxPaint{}}, _, State ) -> {ok}.

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
  %io:format("get move ~n"),
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
      NewMsgState = srcSelected,
      NewState = State#state{src = NewSrc,msgState = NewMsgState},
      wxStaticText:setLabel(SrcTextBox,"Source: " ++ NewSrc),
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
          Pid = queueKeyToPid(NewDst,0),
          wxButton:enable(State#state.sendMsg),
          wxButton:enable(State#state.removeDstBtn),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
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
      NewMsgState = srcSelected,
      wxStaticText:setLabel(SrcTextBox,"Source: " ++ NewSrc),
      wxButton:enable(State#state.removeSrcBtn),
      NewState = State#state{src = NewSrc,msgState = NewMsgState},
      {noreply, NewState};
    srcSelected ->
      NewDst = wxListBox:getStringSelection(Q2_Node_List),
      if
        State#state.src == NewDst ->
          io:format("wrong choice mate ~n"),
          {noreply,State};
        true ->
          %%TODO: change the key that is returned to an actual PID in the real version
          Pid = queueKeyToPid(NewDst,0),
          wxButton:enable(State#state.sendMsg),
          wxButton:enable(State#state.removeDstBtn),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
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
      NewMsgState = srcSelected,
      wxStaticText:setLabel(SrcTextBox,"Source: " ++ NewSrc),
      wxButton:enable(State#state.removeSrcBtn),
      NewState = State#state{src = NewSrc,msgState = NewMsgState},
      {noreply, NewState};
    srcSelected ->
      NewDst = wxListBox:getStringSelection(Q3_Node_List),
      if
        State#state.src == NewDst ->
          io:format("wrong choice mate ~n"),
          {noreply,State};
        true ->
          %%TODO: change the key that is returned to an actual PID in the real version
          Pid = queueKeyToPid(NewDst,0),
          wxButton:enable(State#state.removeDstBtn),
          wxButton:enable(State#state.sendMsg),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
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
      NewMsgState = srcSelected,
      wxStaticText:setLabel(SrcTextBox,"Source: " ++ NewSrc),
      wxButton:enable(State#state.removeSrcBtn),
      NewState = State#state{src = NewSrc,msgState = NewMsgState},
      {noreply, NewState};
    srcSelected ->
      NewDst = wxListBox:getStringSelection(Q4_Node_List),
      if
        State#state.src == NewDst ->
          io:format("wrong choice mate ~n"),
          {noreply,State};
        true ->
          %%TODO: change the key that is returned to an actual PID in the real version
          Pid = queueKeyToPid(NewDst,0),
          wxButton:enable(State#state.removeDstBtn),
          wxButton:enable(State#state.sendMsg),
          case maps:is_key(NewDst,DstMap) of
            false ->
              NewDstMap = maps:put(NewDst,Pid,DstMap),
              wxListBox:append(DestinationComboBox,NewDst),
              NewState = State#state{destinations = NewDstMap},
              {noreply, NewState};
            true -> {noreply,State}
          end
      end
  end;

%% mouse click to pick destination - master sends drone
handle_event(#wx{event=#wxMouse{type = left_down}=_Vec}, State = #state{}) -> {noreply, State};

%% send Message event
handle_event(#wx{obj = SendMsgBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{sendMsg = SendMsgBtn,src = Src,destinations = Destinations,msg = MsgTextBox,msgID = MsgId,locationMap = LocationsMap}) ->
  Msg = wxTextCtrl:getLabel(MsgTextBox),
  DestinationsList = maps:to_list(Destinations),
  OutputList = [Y || {_X,Y} <- DestinationsList],
  insertToETS(LocationsMap),
  case length(OutputList) of
    0 -> {noreply,State};
    1 ->
      gen_server:cast(rplServer,{sendUnicastMessage,MsgId,Src,OutputList,Msg}), %{sendUnicastMessage, From, To, Msg}
      {noreply,State#state{msgID = MsgId + 1}};
    _ ->
      MulticastMessageFormat = makeMulticast(MsgId,OutputList,Src,Msg),
      gen_server:cast(rplServer,{sendUnicastMessage,MulticastMessageFormat}), %%send multicast
      {noreply,State#state{msgID = MsgId + length(OutputList)}}
  end;




%% remove dst node
handle_event(#wx{obj = RemoveDst, event = #wxCommand{type = command_button_clicked}},
    State = #state{destinationCombobox = DestinationComboBox,removeDstBtn = RemoveDst,destinations = Destinations}) ->
  NodeToRemove = wxListBox:getStringSelection(DestinationComboBox),
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
    State = #state{frame = _Frame,removeSrcBtn = RemoveSrc,srcTextBox = SrcTextBox,destinationCombobox = DestinationComboBox,removeDstBtn = RemoveDst}) ->
  wxStaticText:setLabel(SrcTextBox,"Source: null"),
  resetQueue(DestinationComboBox,false),
  wxButton:disable(RemoveSrc),
  wxButton:disable(RemoveDst),
  wxButton:disable(State#state.sendMsg),
  NewState = State#state{src = nullptr,destinations = #{},msgState = notStarted},
  {noreply,NewState};

%%create root event
handle_event(#wx{obj = NewRootBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{frame = _Frame,newRootBtn = NewRootBtn}) ->
  {Result,StateA} = create(root,State),
  if
    Result == error -> {noreply,State} ;
    true ->
      {_DrawStatus,StateB} = draw(StateA),
      {noreply,StateB}
  end;
%%create node event
handle_event(#wx{obj = NewNodeBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{newNodeBtn = NewNodeBtn}) ->
  {Result,StateA} = create(node,State),
  if
    Result == error -> {noreply,State} ;
    true ->
      {_DrawStatus,StateB} = draw(StateA),
      {noreply,StateB}
  end.

%% Callbacks handled as normal gen_server callbacks
handle_info(_Msg, State) ->
  %io:format("Got Info ~p~n",[Msg]),
  {noreply, State}.

handle_call({draw}, _From, State) ->
  draw(State),
  %io:format("got draw ~n"),
  Reply = ok,
  {reply,Reply,State}.

handle_cast(draw, State) ->
  {_Reply,NewState} = draw(State),
  %io:format("got draw ~n"),
  {noreply,NewState};

handle_cast({pathList,PathList},State) ->
  NewState = State#state{pathList = PathList},
  {noreply,NewState};
%%data Rx from drones
handle_cast(_Msg, State) ->
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  {k,v} -> {Pid,{Ref,NumOfRoots,_DontCare,Func,MovementType,NewDirection,{NewX,NewY}}
insertToETS([{Pid,{_Ref,_NumOfRoots,_Type,_Func,_MovementType,_NewDirection,{X,Y}}}|[]]) -> ets:insert(?NODE_LIST,{Pid,{X,Y}});
insertToETS([{Pid,{Ref,_NumOfRoots,_Type,_Func,_MovementType,_NewDirection,{X,Y}}}|T]) ->
  ets:insert(?NODE_LIST,{Pid,{Ref,{X,Y}}}),
  insertToETS(T).

makeMulticast(_MsgId,[],_Src,_Msg) -> [];
makeMulticast(MsgId,OutputList,Src,Msg) ->
  [{MsgId,Src,hd(OutputList),Msg} | makeMulticast(MsgId + 1,Src,tl(OutputList),Msg)].



removeNodeFromListBox(Node,ListBox) ->
  DstList = listBoxToList(ListBox,false),
  NewList = [X || X <- DstList, X =/= Node],
  lists:foreach(fun(X) -> wxListBox:append(ListBox,X) end,NewList),
  io:format("~p~n",[NewList]).

listBoxToList(_ListBox,true) -> [];
listBoxToList(ListBox,false) ->
  Node = wxListBox:getString(ListBox,0),
  io:format("~p~n",[Node]),
  wxControlWithItems:delete(ListBox,0),
  [Node | listBoxToList(ListBox,wxListBox:isEmpty(ListBox))].

queueKeyToPid(Key,0) ->
  queueKeyToPid(tl(Key),1);
queueKeyToPid(Key,1) ->
  queueKeyToPid(tl(Key),2);
queueKeyToPid(Key,2) -> Key.


integer_to_string(Integer) when is_integer(Integer) ->
  lists:flatten(io_lib:format("~p", [Integer])).

init_layout(Mode,Node) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx,-1,"RPL Simulation",[{size,{?MapSize + 500,?MapSize + 500}}]),
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
  SrcTxt = wxStaticText:new(Frame,?wxID_ANY,"Source : null",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),


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
  QueueSizer = wxBoxSizer:new(?wxHORIZONTAL),
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
  wxSizer:add(QueueSizer,_List_Q_1_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(QueueSizer,_List_Q_2_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(QueueSizer,_List_Q_3_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(QueueSizer,_List_Q_4_Label),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  wxSizer:add(CreateSizer,MovementLabel,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,MovementChooser,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,NewRootBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,NewNodeBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MessageSizer,MessageTextBox,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MessageSizer,SendMsgBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MessageSizer,MessageSizer_2,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),

  wxBoxSizer:add(ControlSizer,CreateSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,MessageSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,QueueSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,StartEndSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MainSizer,ControlSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MainSizer,Panel,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),

  %% connect all elements %%
  wxFrame:connect(Frame,close_window),
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
  %wxPanel:connect(Panel, paint, [callback]),
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
    srcTextBox = SrcTxt, msgID = 0, pathList = []
  }.

createRoot(_State) -> io:format("create root ~n").
createNode(_State) -> io:format("create node ~n").

newMessage(_State) -> io:format("new message ~n").

quit(_State) -> io:format("quit ~n").

drawSim(_State) -> 1.


create(RootOrNode, State = #state{locationMap = LocationMap,numOfRoots = NumOfRoots,numOfNodes = NumOfNodes})  ->
  {Func,Type} = case State#state.moveType of
                  "Random" -> {random,random};
                  "Polynomial" -> {funcGenerator:generatePolynom(rand:uniform(3),[]),polynomial};
                  "Sinusoidal" -> {funcGenerator:generateSin(rand:uniform(500),[]),sinusoidal};
                  _ -> badArguement
                end,
  X = rand:uniform(?MapSize),
  {FinalX,FinalY} = case Type of
                      random     -> {rand:uniform(?MapSize),rand:uniform(?MapSize)};
                      polynomial -> getStartingPos(Func,Type,X,RootOrNode, State);
                      sinusoidal -> getStartingPos(Func,Type,X,RootOrNode, State)
                    end,
  %Pid = rand:uniform(100),
  %Ref = rand:uniform(100),
  % NewLocMap = maps:put(Pid,{Ref,NumOfRoots,root,Func,Type,?incerement,{0,0}},LocationMap),
  %drawNodes(maps:to_list(NewLocMap),State#state.panel),
  %drawNode(erlang:element(7,maps:get(Pid,NewLocMap)),State#state.panel),
  NewState = case RootOrNode of
               root ->
                 {Pid,Ref} = gen_server:call(rplServer, {addNode, root}),
                 %gen_server:call(rplServer, {addNode, root}),
                 NewLocMap = maps:put(Pid,{Ref,NumOfRoots,root,Func,Type,?incerement, {FinalX,FinalY}},LocationMap),
                 State#state{locationMap = NewLocMap,numOfRoots = NumOfRoots + 1};
               node ->
                 {Pid,Ref} = gen_server:call(rplServer, {addNode, normal}),
                 NewLocMap = maps:put(Pid,{Ref,NumOfRoots,node,Func,Type,?incerement, {FinalX,FinalY}},LocationMap),
                 State#state{locationMap = NewLocMap,numOfNodes = NumOfNodes + 1}
             end,
  if
    RootOrNode == root -> {root_OK,NewState};
    true -> {node_OK,NewState}
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
    true -> {X,Y}
  end.


draw(State = #state{locationMap = LocationMap, panel = Panel,frame = _Frame}) ->
  %io:format("entered draw ~n"),
  LocationsList = maps:to_list(LocationMap),
  %io:format("erase circles for: ~p~n",[LocationsList]),
  %wxFrame:clearBackground(Panel),
  eraseAllOldLocs(Panel),
  %io:format("enter updateCoordinates ~n"),
  UpdatedLocationMap = updateAllCoordinates(LocationsList,LocationMap),
  %io:format("exited updateAllCoordinates ~n"),
  LocUpdateState = State#state{locationMap = UpdatedLocationMap},
  NewState =  updateQueues(LocUpdateState),
  %io:format("exited getupdates list ~n"),
  %drawNode({rand:uniform(1000),rand:uniform(1000)},Panel),
  drawNodes(maps:to_list(UpdatedLocationMap),Panel),
  %io:format("exited drawNodes ~n"),
  {draw_OK,NewState}.

resetQueue(_,true) -> {ok};
resetQueue(Q_n,false) ->
  wxControlWithItems:delete(Q_n,0),
  Flag = wxListBox:isEmpty(Q_n),
  resetQueue(Q_n,Flag).

updateQueues(State = #state{node_list_q_4 = Q4,node_list_q_3 = Q3,node_list_q_2 = Q2,node_list_q_1 = Q1,locationMap = LocationMap,nq1 = Nq1,nq2 = Nq2,nq3 = Nq3,nq4 = Nq4}) ->
  resetQueue(Q1,wxListBox:isEmpty(Q1)),
  resetQueue(Q2,wxListBox:isEmpty(Q2)),
  resetQueue(Q3,wxListBox:isEmpty(Q3)),
  resetQueue(Q4,wxListBox:isEmpty(Q4)),
  LocationList = maps:to_list(LocationMap),
  {NewQ1,NewQ2,NewQ3,NewQ4} = insertToQueues(LocationList,Q1,Q2,Q3,Q4),
  wxStaticText:setLabel(Nq1,integer_to_string(wxListBox:getCount(Q1))),
  wxStaticText:setLabel(Nq2,integer_to_string(wxListBox:getCount(Q2))),
  wxStaticText:setLabel(Nq3,integer_to_string(wxListBox:getCount(Q3))),
  wxStaticText:setLabel(Nq4,integer_to_string(wxListBox:getCount(Q4))),
  State#state{node_list_q_1 = NewQ1,node_list_q_2 = NewQ2,node_list_q_3 = NewQ3,node_list_q_4 = NewQ4}.

insertToQueues([],Q1,Q2,Q3,Q4) ->
  %io:format("updated Queues ~n"),y`
  {Q1,Q2,Q3,Q4};
insertToQueues(LocationList,Q1,Q2,Q3,Q4) ->
  {Pid,{_Ref,_NumOfRoots,_NodeType,_Func,_Type,_Direction,{X,Y}}}= hd(LocationList),
  AtomString = case _NodeType of
                 root -> "r_";
                 node -> "n_"
               end,
  PidString = pid_to_string(Pid),
  ListKey = AtomString ++ PidString,
  case {X,Y}  of
    {X1,Y1} when X1 >= 500 andalso Y1 >= 500 ->
      wxListBox:append(Q1,[ListKey]),
      insertToQueues(tl(LocationList),Q1,Q2,Q3,Q4);
    {X2,Y2} when X2 < 500 andalso Y2 > 500 ->
      wxListBox:append(Q2,[ListKey]),
      insertToQueues(tl(LocationList),Q1,Q2,Q3,Q4);
    {X3,Y3} when X3 =< 500 andalso Y3 =< 500 ->
      wxListBox:append(Q3,[ListKey]),
      insertToQueues(tl(LocationList),Q1,Q2,Q3,Q4);
    _ ->
      wxListBox:append(Q4,[ListKey]),
      insertToQueues(tl(LocationList),Q1,Q2,Q3,Q4)
  end.

pid_to_string(Pid) when is_pid(Pid) ->
  %io:format("~p~n" ,[Pid]),
  PidStr = pid_to_list(Pid),
  PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
  [N, P1, P2] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],
  {N, P1, P2};
pid_to_string(Pid) when is_integer(Pid) ->
  %io:format("~p ~p *********  ~n",[Pid,integer_to_string(Pid)]),
  integer_to_string(Pid).
updateAllCoordinates([H|[]],LocationMap) ->
  %io:format("enter updateCoordinate: ~p ~n",[H]),
  updateCoordinates(H,LocationMap);
updateAllCoordinates(LocationList,LocationMap) ->
  NewMap = updateCoordinates(hd(LocationList),LocationMap),
  updateAllCoordinates(tl(LocationList),NewMap).



eraseAllOldLocs(Panel) -> eraseNodes(Panel).




drawNodes([{_,{_,_,_,_,_,_,{X,Y}}}|[]],Panel) ->
  %io:format("~p~n",[{X,Y}]),
  drawNode({X,Y},Panel);

drawNodes(LocationsList,Panel) ->
  {X,Y} = getXY(hd(LocationsList)),
  drawNode({X,Y},Panel),
  drawNodes(tl(LocationsList),Panel).

getXY({_,{_,_,_,_,_,_,{X,Y}}}) -> {X,Y}.

drawNode({X,Y},Panel) ->
  Paint = wxPaintDC:new(Panel),
  %Brush = wxBrush:new(?wxBLACK),
  Brush = wxBrush:new(?wxBLACK,[{style,?wxTRANSPARENT}]),
  %%wxBrush:setColour(Brush, ?wxBLUE),
  %%wxBrush:setColour(Brush, 10,10,30),
  wxDC:setBrush(Paint,Brush),
  %WxG=wxGraphicsContext:create(),
  %%wxGraphicsContext:drawEllipse(WxG,100,100,100,100),
  wxDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  %io:format("drawing circle~n"),
  wxBrush:setColour(Brush, ?wxSheikBlue),
  wxDC:setBrush(Paint,Brush),
  wxDC:drawCircle(Paint ,{X,Y},?radius - 1),  %draw circle center at {X,Y}
  wxPaintDC:destroy(Paint).

eraseNodes(Panel) ->
  Paint = wxPaintDC:new(Panel),
  Brush = wxBrush:new(?wxSheikBlue),
  %%wxBrush:setColour(Brush, ?wxBLUE),
  %%wxBrush:setColour(Brush, 10,10,30),
  wxDC:setBrush(Paint,Brush),
  %WxG=wxGraphicsContext:create(),
  %%wxGraphicsContext:drawEllipse(WxG,100,100,100,100),
  %wxDC:drawCircle(Paint ,{X,Y},?radius+1),  %draw circle center at {X,Y}
  %wxDC:drawCircle(Paint ,{X,Y},?radius+10),  %draw circle center at {X,Y}
  wxDC:drawRectangle(Paint,{0,0},{?MapSize,?MapSize}),
  %wxDC:drawCircle(Paint ,{X,Y},?radius),  %draw circle center at {X,Y}
  wxPaintDC:destroy(Paint).

updateCoordinates({Pid,{Ref,NumOfRoots,_DontCare,Func,MovementType,Direction,{X,Y}}},LocationMap) ->
  %io:format("~p~n",[MovementType]),
  {NewX,NewY,NewDirection} = case MovementType of
                               random -> getRandomCoordinates(X,Y);
                               polynomial -> getPolynomialCoordinates(Func,Direction,X);
                               sinusoidal -> getSinusoidalCoordinates(Func,Direction,X)
                             end,
  %io:format("exited getXcoordinates  old:~p   new:  ~p~n", [{X,Y},{NewX,NewY}]),
  NewLocationsMap = maps:update(Pid,{Ref,NumOfRoots,_DontCare,Func,MovementType,NewDirection,{NewX,NewY}},LocationMap),
  NewLocationsMap.

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

transformXtoT(X) -> 2*X - 1000.
transformTtoX(T) -> (T/2) + 500.