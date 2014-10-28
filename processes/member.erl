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
               sms ! {send, Number, "Your membership is expired"};
            true ->
               {Year,Month,Day} = calendar:gregorian_days_to_date(Expiry),
               Valid = io_lib:format("Your membership is valid until ~p/~p/~p", [Year,Month,Day]),
               sms ! {send, Number, lists:flatten(Valid)}
         end;
      balance ->
         Balance = io_lib:format("Your balance is $~p", [db:get_balance(Number)]),
         sms ! {send, Number, lists:flatten(Balance)}
   end.