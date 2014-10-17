-module(unknown).
-export([init/1, loop/3]).

init({Number, [Duration]}) ->
   io:format("only duration: ~p~n", [Duration]),
   Expiry = expiry(Duration),
   loop(Number, Expiry, [""]);

init({Number, [Duration | Name]}) ->
   io:format("duration: ~p and name: ~p~n", [Duration,Name]),
   Expiry = expiry(Duration),
   loop(Number, Expiry, Name).

% main process loop, wait for confirm/deny
loop(Number, Expiry, Name) ->
   receive
      approved ->
         save_member(Number, Expiry, Name),
         % send SMS notification
         io:format("(U): signup approved~n");
      denied ->
         % send SMS notification
         io:format("(U): signup denied~n")
   after 600000 -> % 10 minute timeout
      io:format("(U): signup request timed out~n")         
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

% store new member information in db
save_member(Name, Number, Expiry) ->
   FullName = lists:concat(lists:flatmap(fun(X)->[X," "] end, Name)),
   sqlite3:open(main),
   sqlite3:write(main, person,[
      {name, FullName},
      {phone, Number},
      {expiry, Expiry},
      {role, "member"},
      {balance, 0.0}
   ]),
   sqlite3:close(main).

