% this process handles messages from people
% and dispatches requisite processes

-module(shop).
-export([loop/1]).

% lists to map numbers to days-of-week and back
days_list(name) ->
   [{"monday",1},{"tuesday",2},{"wednesday",3},{"thursday",4},{"friday",5},{"saturday",6},{"sunday",7}];
days_list(number) ->
   [{1,"monday"},{2,"tuesday"},{3,"wednesday"},{4,"thursday"},{5,"friday"},{6,"saturday"},{7,"sunday"}].

% map day of week to a number in [1..7]
day_name_to_number(Day) ->
   case lists:keyfind(Day, 1, days_list(name)) of
      false -> 
         false;
      Idx ->
         {_,Index} = Idx,
         Index
   end.

% produces a message to inform which days a volunteer is signed up for
signed_up_days(Number) ->
   case db:get_days(Number) of
      [] -> "You're not signed up for any shifts.";
      Days ->
         lists:concat([
            "You're signed up for ",
            greetings:concatenate(Days),
            "."
         ])
   end.

loop(Present) ->
   receive
      % add a day of week to volunteer's schedule
      {{_,Number,volunteer,_,_,_}, add, Days} ->
         DayIndices = [day_name_to_number(D) || D <- Days],
         [db:add_day(Number,D) || D <- DayIndices, D =/= false],
         sms!{send, Number, signed_up_days(Number)},
         loop(Present);
         
      % remove a day of week from volunteer's schedule
      {{_,Number,volunteer,_,_,_}, remove, Days} ->
         DayIndices = [day_name_to_number(D) || D <- Days],
         [db:remove_day(Number,D) || D <- DayIndices, D =/= false],
         sms!{send, Number, signed_up_days(Number)},
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
               sms ! {send, Number, greetings:hello()},
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
               db:set_presence(Number,false),
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
               [ sms!{send, N, "Someone is locked out downstairs!" }
               || N <- maps:keys(Present) ],
               sms!{send,Number,"Not again! Hang tight, someone will be down in a minute."};
            false -> 
               sms!{send,Number,"Sorry, no one is here right now.."}
         end,
         loop(Present);

      % schedule query: responds with list of days
      % and which volunteers are signed up for them
      {{_,Number,_,_,_,_}, Action, []} when
            Action == h;
            Action == schedule;
            Action == hours ->
         Days = [[H-32|T] || {[H|T],_} <- days_list(name)],
         Names = [db:get_schedule_day(D) || D <- lists:seq(1,7)],
         Schedule = lists:zip(Names, Days),
         Message = [
            lists:concat([D," - ", N, [10]]) ||
            {N,D} <- Schedule, N =/= []
         ],
         if
            Message == [] ->
               sms!{send,Number,"The schedule is empty, no one has signed up."};
            true ->
               sms!{send,Number,lists:flatten(["18h-21h", 10, Message])}
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
               Message = greetings:shut();
            {_,false,true} ->
               Message = greetings:late();
               % todo: https://github.com/MEBG/mebg/issues/19
            {_,false,false} ->
               Message = greetings:closed(Scheduled);
            {_,true,_} ->
               Names = [Name||{_,Name}<-maps:values(Present)],
               Message = greetings:open(Names)
         end,
         sms ! {send, Number, Message},
         loop(Present);

      % default response
      {{_,Number,_,_,_,_},_,_} ->
         Message = [
            "Mile End Bike Garage, 135 rue Van Horne, 2nd floor. ",
            "Open 6pm to 9pm on weekdays. Visit http://bikegarage.org for more information."
            ],
         sms ! {send, Number, lists:concat(Message)},
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
