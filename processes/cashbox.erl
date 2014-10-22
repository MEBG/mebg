% this process represents the cashbox

-module(cashbox).
-export([loop/1]).

loop(Balance) ->
   receive
      {deposit, Amount} ->
         db:store_transaction(Amount, Balance + Amount),
         loop(Balance + Amount);
      {signup, "year"} ->
         db:store_transaction(20, Balance + 20),
         loop(Balance + 20);
      {signup, "month"} ->
         db:store_transaction(5, Balance + 5),
         loop(Balance + 5);
      {Pid, withdraw, Amount} when Amount < 0 ->
         Pid ! {cashbox, Balance},
         loop(Balance);
      {Pid, withdraw, Amount} when Amount =< Balance ->
         db:store_transaction(Amount, Balance - Amount),
         Pid ! {cashbox, Balance - Amount},
         loop(Balance - Amount);
      {Pid, withdraw, _} when Balance =< 0 ->
         Pid ! {cashbox, Balance},
         loop(Balance);
      {Pid, withdraw, _} ->
         Pid ! {cashbox, Balance},
         loop(Balance);
      {Pid, balance} ->
         Pid ! {cashbox, Balance},
         loop(Balance);
      die -> 1/0;
      balance ->
         io:format("$~p~n",[Balance]),
         loop(Balance)
   end.
