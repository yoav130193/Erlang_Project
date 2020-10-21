
% ***********   DIO MSG   ***********%
-define(DOWNWARD_DIGRAPH, downwardDigraph).
-define(DOWNWARD_DIGRAPH_FILE, "downward_digraph_file.txt").
-define(LOG_FILE_NAME, "my_log_file.txt").
-define(MOP, mop).
-define(MSG_TABLE, msgTable).
-define(MY_DODAGs, my_Dodags).
-define(NODE_LIST, nodeList).
-define(NODE_SERVER, nodeServer).
-define(NON_STORING, 1).
-define(PARENT, parent).
-define(ROOT_LIST, rootList).
-define(ROOT_SERVER, rootServer).
-define(RPL_REF, rplRef).
-define(RPL_SERVER, rplServer).
-define(SERVER, rplServer).
-define(STORING, 0).
-define(Update, 0).
-define(Update_Addition, 2).
-define(Update_NO, 1).
-define(VERSION_RANK, version_rank).
-define(dis, 75).
-define(GFX_SERVER,gfx_server).
-define(APP_SERVER,app_test).
-define(NODE_1,node1).
-define(NODE_2,node2).
-define(NODE_3,node3).
-define(NODE_4,node4).
-define(etsServer,etsServer).
-define(locationEts,nodeList).
-define(nodePidsEts,nodePidsEts).
-define(pidStringEts,pidStringEts).
-define(nodeControlETS,nodeControlEts).
-define(pathEts,pathEts).
-define(MapSize, 750).
-define(GridMapSize,?MapSize div 2).
-define(wxPurple,{16#73,16#26,16#4D,16#FF}).
-define(wxDarkGreen,{16#1F,16#60,16#40,16#FF}).
-define(wxYellow,{16#FF,16#FF,16#4D,16#FF}).
-define(wxOrange,{16#FF,16#99,16#00,16#FF}).
-define(wxDarkCyan,{16#00,16#99,16#99,16#FF}).
-define(wxSheikBlue,{16#F1,16#F9,16#FB,16#FF}).
-define(RandMovement,5).
-define(polynomDelta,1).
-define(sinDelta,2).
-define(incerement,1).
-define(decrement,-1).
-define(radius,(?MapSize div 10)).
-define(appEts,appEts).
-define(R_NODE,'r_node@amirs-MacBook-Pro').
-define(M_NODE,'m_node@amirs-MacBook-Pro').
-define(G_NODE,'g_node@amirs-MacBook-Pro').
-define(N_NODE,'n_node@amirs-MacBook-Pro').

-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId, updateType}).
-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId, updateType}).
-record(dioMsg, {rplInstanceId, versionNumber, rank, g = 2#1, zero = 2#0, mop, prf = 2#000, dtsn, flags = 16#00, reserved = 16#00, dodagId}).
-record(messageFormat, {msgId, from, to, msg}).
-record(msg_table_key, {dodagId, from, to}).
-record(rplServerData, {nodeCount, rootCount, randomLocationList, msg_id, messageList, mop}).
-record(state,{appState,newRootBtn,movementList,nodeTypetoCreate,newNodeBtn,sendMsg,moveType,quit,
  node_list_q_1,node_list_q_2,node_list_q_3,node_list_q_4,nq1,nq2,nq3,nq4,panel,size,frame,
  protocolServer,msgTextBox,mode,node,id,locationMap,msg,numOfNodes,numOfRoots,tableID,msgState,
  src,destinations,srcTextBox,destinationCombobox,startBtn,storing_checkbox,nonStoring_checkbox,
  removeSrcBtn,removeDstBtn,msgID,pathList,debug,createdFirst}).