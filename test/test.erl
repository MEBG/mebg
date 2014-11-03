-module(test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


init() ->
   keepalive:init(test),
   register(test_relay, spawn(test,relay,[void])).

go() ->
   init(),
   schedule_test:test().

relay(TrapId) ->
   receive
      {pid,Pid} ->
         relay(Pid);
      {send,Number,Message} when void =/= TrapId ->
         TrapId!{Number,Message},
         relay(void)
   end.

trap(Timeout, TriggerFun) ->
   test_relay ! {pid,self()},
   timer:sleep(50),
   TriggerFun(),
   receive
      {Number,Message} ->
         {Number,Message}
   after Timeout ->
      error
   end.

send(Number,Message) ->
   trap(100, fun() -> 
      rcvr!{lists:flatten(["+"|Number]), Message} end
   ).
