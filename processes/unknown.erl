-module(unknown).
-export([loop/0]).

loop() ->
   receive
      {Number, "signup", Arguments} ->
         io:format("(U): ~p requesting signup: ~p~n", [Number, Arguments]);
      {Number, Action, Arguments} ->
         io:format("(U): ~p called ~p with ~p~n", [Number, Action, Arguments]),
         loop()
   end.