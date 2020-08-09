%%%-------------------------------------------------------------------
%%% @author amir
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2020 3:55 PM
%%%-------------------------------------------------------------------
-module(graphicsServer).
-author("amir").

-behaviour(wx_object).


-import(util,[integer_to_atom/1]).
-export([start/1, init/1, terminate/2, code_change/3,
  handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3, atom_to_string/1,start_global/1]).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

-define(ImageSizeX, 15).
-define(ImageSizeY, 11).
-define(DEFAULTSPEED, 100).
-define(MapSize, 500).
-define(DroneSize, 20).
-define(SERVER,?MODULE).
-define(GRAPH_ACC,5).
-define(max_x,(2048)).
-define(max_y,(1024)).

-record(state,{appState,newRootBtn,movementList,nodeTypetoCreate,newNodeBtn,sendMsg,moveType,quit,
  node_list_q_1,node_list_q_2,node_list_q_3,node_list_q_4,panel,size,frame,protocolServer,msgTextBox,mode,node}).


%%-record(state,
%%{masterRem,
%%memberRem,
%%clearAllBtn,
%%masterCre,
%%memberCre,
%%dronesList,
%%goToBtn,
%%gatherBtn,
%%mapChooser,
%%speedBtn,
%%currID,
%%dronesLocs,
%%panel,
%%size
%%, frame, goTo, back, target, vel, speedEdit, currMap, obs, hoverBtn, followBtn, drawDrones, memDrone, masDrone, masDroneBit, memDroneBit, send_drone, in_follow, mode, appNode, node}).

start_global(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[global,Node],[]).

start(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[local,Node],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Mode,Node]) ->
  InitState = init_layout(Mode,Node),
  {InitState#state.frame,InitState}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Re-Paint Event - called by refresh
handle_sync_event(#wx{event=#wxPaint{}}, _, State ) ->
  drawSim(State).

%%map choose event
handle_event(#wx{event = #wxCommand{type = command_button_clicked},userData = UserData},State = #state{frame =  Frame}) ->
  case UserData of
    newRoot -> createRoot(State);
    newNode -> createNode(State);
    newMsg  -> newMessage(State);
    quit    -> quit(State)
  end;

handle_event(_Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~n"),
  {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply,Reply,State}.

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
integer_to_string(Integer) when is_integer(Integer) ->
  lists:flatten(io_lib:format("~p", [Integer])).

atom_to_string(Atom) when is_atom(Atom) ->
  integer_to_string(util:atom_to_integer(Atom)).


drawMap("",_,{_,_}) -> ok;

drawMap("Middle",DC,{W,H}) ->
  wxDC:drawRectangle(DC, {100, 100, 300,50}),
  drawFrame(DC,{W,H});

drawMap("Borders",DC,{W,H}) ->
  drawFrame(DC,{W,H});

drawMap("Gap",DC,{W,H}) ->
  wxDC:drawRectangle(DC, {round(W*2 div 3), round(H div 2),W-10-round(W*2 div 3), 10}),
  wxDC:drawRectangle(DC, {10, round(H div 2),round(H div 3), 10}),
  drawFrame(DC,{W,H}).



drawFrame(DC,{W,H}) ->
  wxDC:drawRectangle(DC, {0, 0, 10,H}),
  wxDC:drawRectangle(DC, {0, 0, W,10}),
  wxDC:drawRectangle(DC, {0, H-10, W,10}),
  wxDC:drawRectangle(DC, {W-10, 0,10,H}).

drawDrone(master,Dr,DC,_,MasBit) ->
  wxDC:drawBitmap(DC,MasBit,Dr);
drawDrone(member,Dr,DC,MemBit,_) ->
  wxDC:drawBitmap(DC,MemBit,Dr).



appointMaster(Appoint, Locs) ->
  [{member,{X,Y,_}}] = lists:filter(fun(X)-> {_,{_,_,ID}}= X ,
    atom_to_string(ID)==Appoint end,Locs),
  RemAppointed = lists:filter(fun({_,{_,_,ID}})-> atom_to_string(ID)/=Appoint end,Locs),
  [{master,{X,Y,'0'}}|RemAppointed].

drawTarget(P = {_,_}, DC) ->
  wxDC:drawCircle(DC,P,?DroneSize div 2);

drawTarget(_,_) -> ok.



init_layout(Mode,Node) ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx,-1,"RPL Simulation"),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ControlSizer = wxBoxSizer:new(?wxVERTICAL),
  ListSizer_one = wxBoxSizer:new(?wxHORIZONTAL),
  ListSizer_two = wxBoxSizer:new(?wxHORIZONTAL),
  ListLabel_one = wxBoxSizer:new(?wxHORIZONTAL),
  ListLabel_two = wxBoxSizer:new(?wxHORIZONTAL),
  List_Label_Combiner = wxBoxSizer:new(?wxVERTICAL), MsgSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ExitSizer= wxBoxSizer:new(?wxVERTICAL),
  Size = ?MapSize,
  Panel = wxPanel:new(Frame,[{size, {Size, Size}},{style,?wxFULL_REPAINT_ON_RESIZE}]),
%{size,{Size,Size}}

  %% mover type
  MovementLabel = wxStaticText:new(Frame,?wxID_ANY,"Movement type",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  wxStaticText:wrap(MovementLabel,-1),
  Choices = ["","Random","Polynomial","sinusoidal"],
  MovementChooser = wxComboBox:new(Frame, length(Choices), [{choices, Choices}]),
  wxComboBox:setToolTip(MovementChooser, "Movement Type"),

  %% buttons
  CreateSizer = wxBoxSizer:new(?wxHORIZONTAL),
  NewRootBtn  = wxButton:new(Frame,?wxID_ANY,[{label,"Create Root"}]),
  %wxButton:disable(NewRootBtn),
  NewNodeBtn  = wxButton:new(Frame,?wxID_ANY,[{label,"Create Node"}]),
  %wxButton:disable(NewNodeBtn),
  SendMsgBtn  = wxButton:new(Frame,?wxID_ANY,[{label,"Send Message"}]),
  %wxButton:disable(SendMsgBtn),
  List_Q_1_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.1",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_Q_2_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.2",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_Q_3_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.3",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_Q_4_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.4",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  Node_list_q_1 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_2 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_3 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_4 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  QuitBtn  = wxButton:new(Frame,?wxID_ANY,[{label,"Quit"}]),
  %wxButton:disable(QuitBtn),
  %%SizerText = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "wxTextCtrl multiline"}]),
  MessageTextBox = wxTextCtrl:new(Frame, ?wxID_ANY, [{size, {100,100}},{value, "~Message Data~"}, {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
  %%wxSizer:add(MsgSizer, MessageTextBox,  [{flag, ?wxEXPAND}]),
  %wxSizer:addSpacer(MainSizer, 10),

  wxSizer:addSpacer(ListLabel_one,10),
  wxSizer:add(ListLabel_one, List_Q_1_Label , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:addSpacer(ListLabel_one,30),
  wxSizer:add(ListLabel_one, List_Q_2_Label , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:addSpacer(ListLabel_two,10),
  wxSizer:add(ListLabel_two, List_Q_3_Label , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:addSpacer(ListLabel_two,30),
  wxSizer:add(ListLabel_two, List_Q_4_Label , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),



  wxSizer:add(CreateSizer, NewRootBtn , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer, NewNodeBtn , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer, MessageTextBox , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer, SendMsgBtn , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(List_Label_Combiner,ListLabel_one,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ListSizer_one, Node_list_q_1, [{flag,?wxALL bor ?wxEXPAND}, {border,5}]),
  wxSizer:add(ListSizer_one, Node_list_q_2, [{flag,?wxALL bor ?wxEXPAND}, {border,5}]),
  wxSizer:add(List_Label_Combiner,ListSizer_one,[{flag,?wxALL},{border,5}]),
  wxSizer:add(List_Label_Combiner,ListLabel_two,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ListSizer_two, Node_list_q_3, [{flag,?wxALL bor ?wxEXPAND}, {border,5}]),
  wxSizer:add(ListSizer_two, Node_list_q_4, [{flag,?wxALL bor ?wxEXPAND}, {border,5}]),
  wxSizer:add(List_Label_Combiner,ListSizer_two,[{flag,?wxALL},{border,5}]),
  wxSizer:add(CreateSizer, QuitBtn, [{flag, ?wxEXPAND}]),
  wxSizer:add(ControlSizer,MovementLabel,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ControlSizer,MovementChooser,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ControlSizer,NewRootBtn,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ControlSizer,NewNodeBtn,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ControlSizer,MessageTextBox, [{flag,?wxALL}, {border,5}]),
  wxSizer:add(ControlSizer,SendMsgBtn, [{flag,?wxALL}, {border,5}]),
  %wxSizer:add(ControlSizer,Node_list_q_1,[{flag,?wxALL}]),
  %wxSizer:add(ControlSizer,Node_list_q_2,[{flag,?wxALL}]),
  %wxSizer:add(ControlSizer,Node_list_q_3,[{flag,?wxALL}]),
  %wxSizer:add(ControlSizer,Node_list_q_4,[{flag,?wxALL}]),
  wxSizer:add(ControlSizer,List_Label_Combiner,[{flag,?wxALL},{border,5},{proportion,5}]),
  %wxSizer:add(ControlSizer,ListSizer_two,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ControlSizer,QuitBtn,[{flag,?wxALL},{border,5}]),


  wxSizer:add(MainSizer,ControlSizer,[{flag,?wxALL},{border,5}]),
  % wxSizer:add(MainSizer,ListSizer,[{flag,?wxALL},{border,5}]),
  wxSizer:add(MainSizer,ExitSizer,[{flag,?wxALL},{border,5}]),
  wxSizer:add(MainSizer,Panel,[{flag,?wxALL},{border,5}]),
  wxSizer:add(ControlSizer, MsgSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxSizer:setSizeHints(MainSizer,Frame),
  wxWindow:setMinSize(Frame,wxWindow:getSize(Frame)),
  wxFrame:connect(Frame,close_window),
  wxButton:connect(NewRootBtn,command_button_clicked),
  wxButton:connect(NewNodeBtn,command_button_clicked),
  wxButton:connect(SendMsgBtn,command_button_clicked),
  wxListBox:connect(Node_list_q_1,command_listbox_selected),
  wxListBox:connect(Node_list_q_2,command_listbox_selected),
  wxListBox:connect(Node_list_q_3,command_listbox_selected),
  wxListBox:connect(Node_list_q_4,command_listbox_selected),
  wxComboBox:connect(MovementChooser, command_combobox_selected),
  wxButton:connect(QuitBtn,command_button_clicked),
  wxPanel:connect(Panel, left_down),
  wxPanel:connect(Panel, paint, [callback]),
  wxFrame:show(Frame),

%%  SendMsgBtn  = wxButton:new(Frame,?wxID_ANY,[{label,"New Message"}]),
%%  QuitBtn     = wxButton:new(Frame,?wxID_ANY,[{label,"Quit"}]),
%%
%%
  %%wxSizer:add(ControlSizer,MovementChooser,[{flag,?wxALL},{border,5}]),
%%  wxSizer:add(CreateSizer, SendMsgBtn, [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
%%  wxSizer:add(CreateSizer, QuitBtn   , [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
%%
%%  %% List Of Nodes

%%  ListSizer = wxBoxSizer:new(?wxHORIZONTAL),
%%  wxSizer:add(ListSizer, Node_list_q_1, [{proportion,1}, {flag,?wxALL bor ?wxEXPAND}, {border,5}]),
%%  wxSizer:add(ListSizer, Node_list_q_2, [{proportion,1}, {flag,?wxALL bor ?wxEXPAND}, {border,5}]),
%%  wxSizer:add(ListSizer, Node_list_q_3, [{proportion,1}, {flag,?wxALL bor ?wxEXPAND}, {border,5}]),
%%  wxSizer:add(ListSizer, Node_list_q_4, [{proportion,1}, {flag,?wxALL bor ?wxEXPAND}, {border,5}]),
%%  %% Nessage textBox
%%  SizerText = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "wxTextCtrl multiline"}]),
%%  MessageTextBox = wxTextCtrl:new(Panel, 3, [{value, "This is a\nmultiline\nwxTextCtrl"}, {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
%%  %% Add to sizers
%%  wxSizer:add(SizerText, MessageTextBox,  [{flag, ?wxEXPAND}]),
%%  wxSizer:addSpacer(MainSizer, 10),
%%   wxSizer:add(CreateSizer, NewRootBtn, [{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
%%  wxSizer:add(MainSizer, SizerText, [{flag, ?wxEXPAND}, {proportion, 1}]),
%%  wxPanel:setSizer(Panel, MainSizer),
%%  %% General configurations
%%  wxSizer:add(MainSizer,ControlSizer,[{flag,?wxALL},{border,5}]),
%%  wxSizer:add(MainSizer,Panel,[{flag,?wxALL},{border,5}]),
%%  wxWindow:setSizer(Frame, MainSizer),
%%  wxSizer:setSizeHints(MainSizer,Frame),
%%  wxWindow:setMinSize(Frame,wxWindow:getSize(Frame)),
%%  wxFrame:connect(Frame,close_window),

%%  wxButton:connect(SendMsgBtn,command_button_clicked),
%%  wxButton:connect(QuitBtn,command_button_clicked),
%%  wxListBox:connect(Node_list_q_1,command_listbox_selected),
%%  wxListBox:connect(Node_list_q_2,command_listbox_selected),
%%  wxListBox:connect(Node_list_q_3,command_listbox_selected),
%%  wxListBox:connect(Node_list_q_4,command_listbox_selected),
%%  wxTextCtrl:connect(MessageTextBox, command_text_enter),
  #state
  {
    frame = Frame, panel = Panel, newRootBtn = NewRootBtn, newNodeBtn = NewNodeBtn, quit = Frame, sendMsg = Frame, node_list_q_1 = Frame,
    node_list_q_2 = Frame, node_list_q_3 = Frame,node_list_q_4 = Frame, movementList = Frame, moveType = "",
    nodeTypetoCreate = "", size = Size, protocolServer = "", appState = initiated, msgTextBox = Frame, mode = Mode, node = Node
  }.



%#state
%{
%frame = Frame, panel = Panel, newRootBtn = NewRootBtn, newNodeBtn = NewNodeBtn, quit = QuitBtn, sendMsg = SendMsgBtn, node_list_q_1 = Node_list_q_1,
%node_list_q_2 = Node_list_q_2, node_list_q_3 = Node_list_q_3,node_list_q_4 = Node_list_q_4, movementList = MovementChooser, moveType = "",
%%nodeTypetoCreate = "", size = Size, protocolServer = "", appState = initiated, msgTextBox = MessageTextBox, mode = Mode, node = Node
%}.




createRoot(State) -> 1.
createNode(State) -> 1.

newMessage(State) -> 1.

quit(State) -> 1.

drawSim(State) -> 1.
