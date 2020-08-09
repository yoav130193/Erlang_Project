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
  node_list_q_1,node_list_q_2,node_list_q_3,node_list_q_4,panel,size,frame,
  protocolServer,msgTextBox,mode,node,id,locationList,msg}).



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
%% Re-Paint Event - called by refresh
handle_sync_event(#wx{event=#wxPaint{}}, _, State ) ->
  drawSim(State).

%%create root event
handle_event(#wx{obj = NewRootBtn, event = #wxCommand{type = command_button_clicked}},
    State = #state{frame = Frame,newRootBtn = NewRootBtn}) ->

%%create node event
  handle_event(#wx{obj = NewNodeBtn, event = #wxCommand{type = command_button_clicked}},
  State = #state{frame = Frame,newNodeBtn = NewNodeBtn}) ->
io:format("Got Event ~n"),
{noreply,State};
handle_event(_Ev = #wx{}, State = #state{}) ->
io:format("Got Event test ~n"),
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
  Panel = wxPanel:new(Frame,[{size,{?MapSize,?MapSize}},{style,?wxFULL_REPAINT_ON_RESIZE}]),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ControlSizer = wxBoxSizer:new(?wxVERTICAL),
  SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL}],
  %% create all UI elements %%
  NewRootBtn = wxButton:new(Frame,?wxID_ANY,[{label,"Create Root"}]),
  NewNodeBtn = wxButton:new(Frame,?wxID_ANY,[{label,"Create Node"}]),
  QuitBtn    = wxButton:new(Frame,?wxID_ANY,[{label,"Quit"}]),
  SendMsgBtn = wxButton:new(Frame,?wxID_ANY,[{label,"Send Message"}]),
  List_Q_1_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.1",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_Q_2_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.2",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_Q_3_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.3",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  List_Q_4_Label = wxStaticText:new(Frame,?wxID_ANY,"Q.4",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  Node_list_q_1 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_2 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_3 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  Node_list_q_4 = wxListBox:new(Frame, ?wxID_ANY, [{size, {50,100}}, {choices, []}, {style, ?wxLB_SINGLE}]),
  MessageTextBox = wxTextCtrl:new(Frame, ?wxID_ANY, [{size, {100,100}},{value, "~Message Data~"}, {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
  MovementLabel = wxStaticText:new(Frame,?wxID_ANY,"Movement type",[{size,{-1,-1}},{style,?wxALIGN_LEFT}]),
  wxStaticText:wrap(MovementLabel,-1),
  Choices = ["","Random","Polynomial","sinusoidal"],
  MovementChooser = wxComboBox:new(Frame, length(Choices), [{choices, Choices}]),
  wxComboBox:setToolTip(MovementChooser, "Movement Type"),

  %% order UI elements in sizers %%
  %% create UI sizers %%
  CreateSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(CreateSizer,MovementLabel,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,MovementChooser,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,NewRootBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxSizer:add(CreateSizer,NewNodeBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  %% create QUEUE sizers %%
  QueueSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ListSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ListLabelSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ListLabelCombiner = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(ListLabelSizer,List_Q_1_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListLabelSizer,List_Q_2_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListLabelSizer,List_Q_3_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListLabelSizer,List_Q_4_Label,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListSizer,Node_list_q_1,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListSizer,Node_list_q_2,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListSizer,Node_list_q_3,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListSizer,Node_list_q_4,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListLabelCombiner,ListLabelSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ListLabelCombiner,ListSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(QueueSizer,ListLabelCombiner),[{flag, ?wxEXPAND bor ?wxALL},{border,5}],
  %% send message sizers %%
  MessageSizer = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(MessageSizer,MessageTextBox,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MessageSizer,SendMsgBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  %% quit sizer %%
  QuitSizer = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(QuitSizer,QuitBtn,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  %% put all sizers together %%
  wxBoxSizer:add(ControlSizer,CreateSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,MessageSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,QueueSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(ControlSizer,QuitSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MainSizer,ControlSizer,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxBoxSizer:add(MainSizer,Panel,[{flag, ?wxEXPAND bor ?wxALL},{border,5}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxSizer:setSizeHints(MainSizer,Frame),
  wxWindow:setMinSize(Frame,wxWindow:getSize(Frame)),
  %% connect all elements %%
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
  %% config ui for start of run %%
  %wxButton:disable(SendMsgBtn),
  %wxButton:disable(NewNodeBtn),
  %wxButton:disable(NewRootBtn),
  %% show frame %%
  wxPanel:connect(Panel, left_down),
  wxPanel:connect(Panel, paint, [callback]),
  wxFrame:show(Frame),
  #state
  {
    frame = Frame, panel = Panel, newRootBtn = NewRootBtn, newNodeBtn = NewNodeBtn, quit = QuitBtn, sendMsg = SendMsgBtn,
    node_list_q_1 = Node_list_q_1, node_list_q_2 = Node_list_q_2, node_list_q_3 = Node_list_q_3,node_list_q_4 = Node_list_q_4,
    movementList = Frame, moveType = "", nodeTypetoCreate = "", size = ?MapSize, protocolServer = "",
    appState = initiated, msgTextBox = Frame, mode = Mode, node = Node, msg = MessageTextBox
  }.

createRoot(State) -> io:format("create root ~n").
createNode(State) -> io:format("create node ~n").

newMessage(State) -> io:format("new message ~n").

quit(State) -> io:format("quit ~n").

drawSim(State) -> 1.
