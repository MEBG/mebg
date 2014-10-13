-module(volunteer).
-export([loop/0]).

loop() ->
   receive
      {Number, "arrive", _} ->
         coop ! {self(), Number, arrive},
         loop();
      {Number, "depart", _} ->
         coop ! {Number, depart};
      {Number, Action, Arguments} ->
         io:format("(V): ~p called ~p with ~p~n", [Number, Action, Arguments]),
         loop()
   end.