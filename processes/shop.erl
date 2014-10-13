% this process represents a physical shop location

-module(shop).
-export([loop/1]).

loop(Present) ->
   receive
      present ->
         io:format("present volunteers: ~p~n", [Present]),
         loop(Present);
      isOpen ->
         io:format("shop open: ~p~n", [maps:size(Present) > 0]),
         loop(Present);
      {Volunteer, Number, arrive} ->
         io:format("arrived"),
         Exists = maps:is_key(Number, Present),
         if
            not Exists ->
               loop(maps:put(Number, Volunteer, Present));
            true ->
               loop(Present)
         end;
      {Number, depart} ->
         Exists = maps:is_key(Number, Present),
         if
            Exists ->
               loop(maps:without([Number], Present));
            true ->
               loop(Present)
         end
   end.
