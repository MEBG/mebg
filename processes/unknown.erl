-module(unknown).
-export([init/1, loop/4]).

init({Number, [Duration]}) ->
   Expiry = expiry(Duration),
   loop(Number, Expiry, Duration, [""]);

init({Number, [Duration | Name]}) ->
   Expiry = expiry(Duration),
   loop(Number, Expiry, Duration, Name).

% main process loop, wait for confirm/deny
loop(Number, Expiry, Duration, Name) ->
   receive
      approved ->
         FullName = lists:concat(lists:flatmap(fun(X)->[X," "] end, Name)),
         db:save_member(Number, Expiry, FullName),
         % update cashbox balance
         box ! {signup, Duration},
         % send SMS notification
         sender:send(Number, "Membership request approved");
      denied ->
         % send SMS notification
         sender:send(Number, "Membership request denied")
   after 600000 -> % 10 minute timeout
      sender:send(Number, "Membership request timed out")
   end.

% calculate expiration date given duration of "month" or "year"
expiry(Duration) ->
   {{Year,Month,Day}, _} = erlang:localtime(),
   Days = calendar:date_to_gregorian_days({Year, Month, Day}),
   if
      Duration == "year" ->
         Days + 365;
      true -> %default of 1 month
         Days + 30
   end.

