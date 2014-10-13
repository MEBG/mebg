% this process represents the cashbox

-module(cashbox).
-export([loop/1]).

loop(Balance) ->
   receive
      {deposit, Amount} ->
         io:format("$~p deposited~n", [Amount]),
         loop(Balance + Amount);
      {withdraw, Amount} when Amount < 0 ->
         io:format("cannot withdraw a negative amount~n"),
         loop(Balance);
      {withdraw, Amount} when Amount =< Balance ->
         io:format("$~p withdrawn~n", [Amount]),
         loop(Balance - Amount);
      {withdraw, _} when Balance =< 0 ->
         io:format("current balance is zero, cannot withdraw~n"),
         loop(Balance);
      {withdraw, _} ->
         io:format("error: cannot withdraw more than current balance~n"),
         loop(Balance);
      balance ->
         io:format("current balance is $~p~n", [Balance]),
         loop(Balance)
   end.
