%%%-------------------------------------------------------------------
%%% @author yoavlevy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2020 14:04
%%%-------------------------------------------------------------------
-module('RPL_Msg').
-author("yoavlevy").

%% API
-export([dioMsg/1]).
-export([daoMsg/2]).
-export([daoAck/1]).
-export([sendUnicastMsg/3]).
-export([sendMulticastMsg/3]).



% DIO (DODAF Indormation Object)
% This is a multicast message from the root that notifies all the nodes about his DODAG
% TODO - check if this message is only from the root and when exactly this happens
dioMsg(Dodag)->Dodag.

%DAO Destination Advertisment Object)
% unicast message from all the node to the root, this message is a request to join the DODAG
% TODO - check if the destination is only the root and if it always happens after DIO
daoMsg(root , Node )->Node.

% DAO acknowledge
% After receiving a DAO message, the root sends an ack message that confirms that the node accepted in to the DODAG
% TODO - Understand if besides root , other nodes sends this message. If yes, the response is different
daoAck(Dodag)-> Dodag.

% Regular unicast Message
% Sending a unicast message to a specific destination
sendUnicastMsg(SourceNode,DestinationNode,Msg)->Msg.

% Regular multicast Message
% Sending a multicast message to a specific DODAG
sendMulticastMsg(SourceNode,DodagList,Msg)->Msg.

buildDodags({NodesPids,X,Y})->