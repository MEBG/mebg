% this process handles messages from people
% and dispatches requisite processes

-module(shop).
-export([loop/1, get_today_name/0]).

get_today_name() ->
   Days = [{1,"monday"},{2,"tuesday"},{3,"wednesday"},{4,"thursday"},{5,"friday"},{6,"saturday"},{7,"sunday"}],
   {Date,_} = erlang:localtime(),
   {_, Today} = lists:keyfind(calendar:day_of_the_week(Date), 1, Days),
   Today.

loop(Present) ->
   receive
      {{_,Number,volunteer,_,_,_}, add, Days} ->
         [db:add_day(Number,D) || D <- Days],
         sms!{send, Number, db:get_days_string(Number)},
         loop(Present);
         
      {{_,Number,volunteer,_,_,_}, remove, Days} ->
         [db:remove_day(Number,D) || D <- Days],
         sms!{send, Number, db:get_days_string(Number)},
         loop(Present);

      % volunteer arrival
      {{_,Number,volunteer,Name,_,_}, Action, _} when
            Action == i;
            Action == in;
            Action == here;
            Action == arrive;
            Action == open ->
         Exists = maps:is_key(Number, Present),
         if
            not Exists ->
               V = spawn(volunteer,loop,[Number]),
               sms ! {send, Number, greetings:hello()},
               db:set_presence(Number,true),
               loop(maps:put(Number, {V,Name}, Present));
            true ->
               loop(Present)
         end;

      % volunteer departure
      {{_,Number,volunteer,_,_,_}, Action, _} when
            Action == o;
            Action == out;
            Action == leave;
            Action == depart;
            Action == close ->
         Exists = maps:is_key(Number, Present),
         if
            Exists ->
               {V,_} = maps:get(Number, Present),
               V ! goodbye,
               db:set_presence(Number,false),
               loop(maps:without([Number], Present));
            true ->
               loop(Present)
         end;

      % approval of member signup request by volunteer
      {{_,Number,volunteer,_,_,_}, approve, _} ->
         Exists = maps:is_key(Number, Present),
         if
            Exists ->
               {V,_} = maps:get(Number, Present),
               V ! approved;
            true -> void
         end,
         loop(Present);

      % denial of member signup request by volunteer
      {{_,Number,volunteer,_,_,_}, deny, _} ->
         Exists = maps:is_key(Number, Present),
         if
            Exists ->
               {V,_} = maps:get(Number, Present),
               V ! denied,
               loop(Present);
            true -> void
         end,
         loop(Present);


      % membership inquiry
      {{_,Number,member,_,_,_}, verify, _} ->
         M = spawn(member, loop, [Number]),
         M ! verify,
         loop(Present);

      % balance inquiry
      {{_,Number,member,_,_,_}, balance, _} ->
         M = spawn(member, loop, [Number]),
         M ! balance,
         loop(Present);

      % signup request from unknown number
     {{_,Number,unknown,_,_,_}, signup, Arguments} ->
         % create a process to wait for response
         U = spawn(unknown, init, [{Number,Arguments}]),
         Open = maps:size(Present) > 0,
         if
            Open ->
               % grab first volunteer (for now)
               [{V,_}|_] = maps:values(Present),
               V ! {U, signup},
               loop(Present);
            true ->
               U ! denied,
               loop(Present)
         end;

      % schedule query
      {{_,Number,_,_,_,_}, schedule, ["today"]} ->
         Vs = db:get_schedule_day(get_today_name()),
         case Vs of
            [] -> Msg = "Noone is";
            V -> Msg = V
         end,
         sms!{send,Number,lists:concat([
               Msg,
               " scheduled for today"
            ])},
         loop(Present);

      % "is the shop open" query
      {{_,Number,_,_,_,_}, Action, _} when
            Action == s;
            Action == status ->
         Open = maps:size(Present) > 0,
         if
            Open ->
               Names = [Name||{_,Name}<-maps:values(Present)],
               Message = greetings:open(Names);
            not Open ->
               Message = greetings:closed()
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
         Names = [Name || {_,Name} <- maps:values(Present)],
         io:format("present volunteers: ~p~n", [Names]),
         loop(Present);
      isOpen ->
         io:format("shop open: ~p~n", [maps:size(Present) > 0]),
         loop(Present);
      {Other} ->
         io:format("shop default unrecognized: ~p~n", [Other]),
         loop(Present)
   end.
