-module(member).
-export([loop/0]).

loop() ->
   receive
      {Number, Action, Arguments} ->
         io:format("(M): ~p called ~p with ~p~n", [Number, Action, Arguments]),
         loop()
   end.