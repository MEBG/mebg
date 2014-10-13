-module(volunteer).
-export([loop/0, name/0]).

name() ->
   {first,last}.

loop() ->
   receive
      {Number, "arrive", _} ->
         coop ! {self(), Number, arrive},
         loop();
      {Number, "depart", _} ->
         coop ! {Number, depart},
         loop();
      {Number, Action, Arguments} ->
         io:format("(V): ~p called ~p with ~p~n", [Number, Action, Arguments]),
         loop()
   end.