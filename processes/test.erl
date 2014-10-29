-module(test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


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

status_test() ->
   Number = "15145551234",
   {Number,Message} = trap(350,fun() -> rcvr!{lists:flatten(["+"|Number]),"status"} end),
   Message.
