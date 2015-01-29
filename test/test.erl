-module(test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% Test shim, allows for testing synchronous messages
% Used to test request-response pairs (eg, when "status" request is sent,
% we expect the system will respond with a message, and want to capture
% that message to compare expected vs actual response)
% Depends on [mode] of sender (sms process) - in test mode, sms routes
% messages via test_send, which uses test_relay registered process

% bootstrap test environment
init() ->
   keepalive:init(test),
   register(test_relay, spawn(test,relay,[void])).

% relays message to trap process
relay(TrapId) ->
   receive
      % set up with id of trap process
      {pid,Pid} ->
         relay(Pid);
      % relay any message to trap process & forget trap process id
      {send,Number,Message} when void =/= TrapId ->
         TrapId!{Number,Message},
         relay(void)
   end.

% trap process - expects to receive & return a message before timeout 
trap(Timeout, SendFunction) ->
   test_relay ! {pid,self()},
   timer:sleep(50),
   SendFunction(),
   receive
      {Number,Message} ->
         {Number,Message}
   after Timeout ->
      error
   end.

% sends synchronous request to Number, returns response message
send(Number, Message) ->
   trap(100, fun() -> 
      rcvr!{lists:flatten(["+"|Number]), Message} end
   ).
