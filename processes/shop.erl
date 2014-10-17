% this process handles messages from people
% and dispatches requisite processes

-module(shop).
-export([loop/1]).

loop(Present) ->
   receive
      % volunteer arrival
      {Number, volunteer, arrive, _} ->
         Exists = maps:is_key(Number, Present),
         if
            not Exists ->
               V = spawn(volunteer,loop,[]),
               loop(maps:put(Number, V, Present));
            true ->
               loop(Present)
         end;
      % volunteer departure
      % what if there's a request pending!?
      {Number, volunteer, depart, _} ->
         Exists = maps:is_key(Number, Present),
         if
            Exists ->
               V = maps:get(Number, Present),
               V ! goodbye,               
               loop(maps:without([Number], Present));
            true ->
               loop(Present)
         end;
      % approval of signup request by volunteer
      {Number, volunteer, approve, _} ->
         Exists = maps:is_key(Number, Present),
         if
            Exists ->
               V = maps:get(Number, Present),
               V ! approved
         end,
         loop(Present);
      % denial of signup request by volunteer
      {Number, volunteer, deny, _} ->
         Exists = maps:is_key(Number, Present),
         if
            Exists ->
               V = maps:get(Number, Present),
               V ! denied,
               loop(Present)
         end,
         loop(Present);

      % signup request from unknown number
      {Number, unknown, signup, Arguments} ->
         io:format("received signup request from new member:"),
         io:format("~p [~p]~n", [Number, Arguments]),
         % create a process to wait for response
         U = spawn(unknown, loop, []),
         Open = maps:size(Present) > 0,
         if
            Open ->
               % grab first volunteer (for now)
               [V|_] = maps:values(Present),
               V ! {U, signup, Arguments},
               loop(Present);
            true ->
               U ! denied,
               loop(Present)
         end;

      % "is the shop open" query
      {_, _, status, _} ->
         io:format("shop open: ~p~n", [maps:size(Present) > 0]),
         loop(Present);

      % catch-all for debugging
      {Number, Role, Action, Arguments} ->
         io:format("received '~p ~p' ", [Action, Arguments]),
         io:format("from ~p (~p)~n", [Number, Role]),
         loop(Present);

      % for in-shell debugging
      present ->
         io:format("present volunteers: ~p~n", [Present]),
         loop(Present);
      isOpen ->
         io:format("shop open: ~p~n", [maps:size(Present) > 0]),
         loop(Present)
   end.
