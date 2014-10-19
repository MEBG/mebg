% this process represents the cashbox

-module(cashbox).
-export([loop/1]).

loop(Balance) ->
   receive
      {deposit, Amount} ->
         loop(Balance + Amount);
      {signup, "year"} ->
         loop(Balance + 20);
      {signup, "month"} ->
         loop(Balance + 5);
      {Pid, withdraw, Amount} when Amount < 0 ->
         Pid ! {cashbox, Balance},
         loop(Balance);
      {Pid, withdraw, Amount} when Amount =< Balance ->
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
         loop(Balance)
   end.
