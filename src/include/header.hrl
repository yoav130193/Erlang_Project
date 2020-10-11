
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

-define(NODE_1,node1).
-define(NODE_2,node2).
-define(NODE_3,node3).
-define(NODE_4,node4).

-record(daoAckMsg, {rplInstanceId, d = 2#1, reserved = 2#0000000, daoSequence, status = 16#01, dodagId, updateType}).
-record(daoMsg, {rplInstanceId, k = 2#1, d = 2#1, flags = 8#00, reserved = 16#00, daoSequence, dodagId, updateType}).
-record(dioMsg, {rplInstanceId, versionNumber, rank, g = 2#1, zero = 2#0, mop, prf = 2#000, dtsn, flags = 16#00, reserved = 16#00, dodagId}).
-record(messageFormat, {msgId, from, to, msg}).
-record(msg_table_key, {dodagId, from, to}).
-record(rplServerData, {nodeCount, rootCount, randomLocationList, msg_id, messageList, mop}).
