-module(unknown).
-export([loop/0]).

loop() ->
   receive
      approved ->
         % send SMS notification
         io:format("(U): signup approved~n");
      denied ->
         % send SMS notification
         io:format("(U): signup denied~n")
   end.