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


send(Number,Message) ->
   trap(100, fun() -> 
      rcvr!{lists:flatten(["+"|Number]), Message} end
   ).

t100_status_test() ->
   Number = "111",
   {Number,Message} = send(Number,"status"),
   [Message] = [M||M<-greetings:closed_phrases(), M == Message].

t200_arrive_test() ->
   Number = "111",
   {Number,M1} = send(Number,"arrive"),
   [M1] = [M||M<-greetings:hello_phrases(), M == M1],
   {Number,M2} = send(Number,"status"),
   [M2] = [lists:flatten(M)||M<-greetings:open_phrases(["TV01"]), lists:flatten(M) == M2].
   
t300_depart_test() ->
   Number = "111",
   {Number,M1} = send(Number,"depart"),
   [M1] = [M||M<-greetings:bye_phrases(), M == M1],
   {Number,M2} = send(Number,"status"),
   [M2] = [M||M<-greetings:closed_phrases(), M == M2].

t310_depart_test() ->
   Number = "111",
   error = send(Number,"depart").

t400_double_arrive_test() ->
   {"111",_} = send("111","arrive"),
   {"222",_} = send("222","arrive"),
   {"111",Message} = send("111","status"),
   [Message] = [lists:flatten(M)||M<-greetings:open_phrases(["TV01", "TV02"]), lists:flatten(M) == Message].

t410_double_depart_test() ->
   {"111",_} = send("111","depart"),
   {"111",M1} = send("111","status"),
   [M1] = [lists:flatten(M)||M<-greetings:open_phrases(["TV02"]), lists:flatten(M) == M1],
   {"222",_} = send("222","depart"),
   {"111",M2} = send("111","status"),
   [M2] = [M||M<-greetings:closed_phrases(), M == M2].
