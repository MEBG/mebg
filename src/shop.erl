% this process handles messages from people
% and dispatches requisite processes

-module(shop).

-export([loop/1]).

days_list() ->
   [{"monday",1},{"tuesday",2},{"wednesday",3},{"thursday",4},{"friday",5},{"saturday",6},{"sunday",7}].
day_name_to_number(Day) ->
   case lists:keyfind(Day, 1, days_list()) of
      false -> false;
      {_, Index} -> Index
   end.

scheduled(Number) -> scheduled(Number, db:get_days(Number)).
scheduled(Number, []) -> sms ! {send, Number, {notsignedup}};
scheduled(Number, Shifts) -> sms ! {send, Number, {days, Shifts}}.

loop(Present) ->
   receive
      % add a day of week to volunteer's schedule
      {{_, Number, volunteer, _, _, _}, add, Days} ->
         DayIndices = [day_name_to_number(D) || D <- Days],
         [db:add_day(Number, D) || D <- DayIndices, D =/= false],
         scheduled(Number),
         loop(Present);

      % remove a day of week from volunteer's schedule
      {{_, Number, volunteer, _, _, _}, remove, Days} ->
         DayIndices = [day_name_to_number(D) || D <- Days],
         [db:remove_day(Number,D) || D <- DayIndices, D =/= false],
         scheduled(Number),
         loop(Present);

      % volunteer arrival: spawns a new volunteer process and adds it to
      % the presence list (in-process and persisted in db)
      {{_,Number,volunteer,Name,_,_}, Action, _} when
            Action == i;
            Action == in;
            Action == here;
            Action == arrive;
            Action == open ->
         case maps:is_key(Number, Present) of
            false ->
               V = spawn(volunteer,loop,[Number]),
               sms ! {send, Number, {hello}},
               db:set_presence(Number,true),
               loop(maps:put(Number, {V,Name}, Present));
            true ->
               loop(Present)
         end;

      % volunteer departure: destroys matching volunteer process and
      % clears number from presence lists (db and in-process)
      {{_,Number,volunteer,_,_,_}, Action, _} when
            Action == o;
            Action == out;
            Action == leave;
            Action == depart;
            Action == close ->
         case maps:is_key(Number, Present) of
            true ->
               {V,_} = maps:get(Number, Present),
               V ! goodbye,
               sms ! {send, Number, {bye}},
               db:set_presence(Number, false),
               loop(maps:without([Number], Present));
            false ->
               loop(Present)
         end;

      % notify on-shift volunteers that the downstairs door is locked
      {{_,Number,_,_,_,_}, Action, _} when
            Action == locked;
            Action == door ->
         case maps:size(Present) > 0 of
            true ->
               [ sms ! {send, N, {door_open} }
               || N <- maps:keys(Present) ],
               sms ! {send, Number, {door_wait}};
            false ->
               sms ! {send, Number, {door_closed}}
         end,
         loop(Present);

      % schedule query: responds with list of days
      % and which volunteers are signed up for them
      {{_,Number,_,_,_,_}, Action, []} when
            Action == h;
            Action == schedule;
            Action == hours ->
         Days = [[H-32|T] || {[H|T],_} <- days_list()],
         Names = [db:get_schedule_day(D) || D <- lists:seq(1,7)],
         Schedule = lists:zip(Names, Days),
         Shifts = [
            lists:concat([D," - ", N, [10]]) ||
            {N,D} <- Schedule, N =/= []
         ],
         if
            Shifts == [] ->
               sms ! {send, Number, {empty}};
            true ->
               sms ! {send, Number, {schedule, Shifts}}
         end,
         loop(Present);

      % returns a message indicating whether the shop is open,
      % if it was supposed to be open and isn't, etc
      {{_,Number,_,_,_,_}, Action, _} when
            Action == s;
            Action == st;
            Action == status ->
         {_,{Hour,_,_}} = erlang:localtime(),
         Within = Hour > 17 andalso Hour < 21,
         Scheduled = db:get_volunteers_today(),
         case {Scheduled, maps:size(Present) > 0, Within} of
            {[],false,_} ->
               Message = {shut};
            {_,false,true} ->
               Message = {late};
               % todo: https://github.com/MEBG/mebg/issues/19
            {_,false,false} ->
               Message = {closed, Scheduled};
            {_,true,_} ->
               Names = [Name||{_,Name}<-maps:values(Present)],
               Message = {open, Names}
         end,
         sms ! {send, Number, Message},
         loop(Present);

      % default response
      {{_,Number,_,_,_,_},_,_} ->
         sms ! {send, Number, {default}},
         loop(Present);

      % for in-shell debugging
      present ->
         io:format("present volunteers: ~p~n", [Present]),
         loop(Present);
      isOpen ->
         io:format("shop open: ~p~n", [maps:size(Present) > 0]),
         loop(Present);
      {Other} ->
         io:format("shop default unrecognized: ~p~n", [Other]),
         loop(Present)
   end.
