-module(member).
-export([loop/1]).

loop(Number) ->
   receive
      verify ->
         Expiry = db:get_expiry(Number),
         {Date,_} = erlang:localtime(),
         Today = calendar:date_to_gregorian_days(Date),
         if
            Today > Expiry ->
               sender:send(Number, "membership expired");
            true ->
               sender:send(Number, "membership valid")
         end;
      balance ->
         % todo: format message string
         sender:send(Number, db:get_balance(Number))
   end.