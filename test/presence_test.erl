-module(presence_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


t100_status_test() ->
   Number = "111",
   {Number,Message} = test:send(Number,"status"),
   [Message] = [M||M<-greetings:closed_phrases(), M == Message].

t200_arrive_test() ->
   Number = "111",
   {Number,M1} = test:send(Number,"arrive"),
   [M1] = [M||M<-greetings:hello_phrases(), M == M1],
   {Number,M2} = test:send(Number,"status"),
   [M2] = [lists:flatten(M)||M<-greetings:open_phrases(["TV01"]), lists:flatten(M) == M2].
   
t300_depart_test() ->
   Number = "111",
   {Number,M1} = test:send(Number,"depart"),
   [M1] = [M||M<-greetings:bye_phrases(), M == M1],
   {Number,M2} = test:send(Number,"status"),
   [M2] = [M||M<-greetings:closed_phrases(), M == M2].

t310_depart_test() ->
   Number = "111",
   error = test:send(Number,"depart").

t400_double_arrive_test() ->
   {"111",_} = test:send("111","arrive"),
   {"222",_} = test:send("222","arrive"),
   {"111",Message} = test:send("111","status"),
   [Message] = [lists:flatten(M)||M<-greetings:open_phrases(["TV01", "TV02"]), lists:flatten(M) == Message].

t410_double_depart_test() ->
   {"111",_} = test:send("111","depart"),
   {"111",M1} = test:send("111","status"),
   [M1] = [lists:flatten(M)||M<-greetings:open_phrases(["TV02"]), lists:flatten(M) == M1],
   {"222",_} = test:send("222","depart"),
   {"111",M2} = test:send("111","status"),
   [M2] = [M||M<-greetings:closed_phrases(), M == M2].
