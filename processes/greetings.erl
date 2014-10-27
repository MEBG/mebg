-module(greetings).
-export([get_random/1, hello/0, bye/0, open/1, closed/0]).

get_random(List) ->
   [R|_] = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
   R.

hello() ->
   M = [
      "Welcome!",
      "Thought you'd never drop by!",
      "It's been a while, pardner",
      "Nice to see you",
      "Where have YOU been?",
      "Welcome to the jungle..",
      "Nice day!",
      "G'day mate",
      "A good day to wrench around..",
      "Uhh, sorry about the mess..",
      "I think we're out of chain breakers..",
      "Bienvenidos compadre!"
   ],
   get_random(M).

bye() ->
   M = [
      "So long!",
      "Thanks for bringin' it brah!",
      "And good riddance to you, sir!.",
      "Til next time!",
      "Good times, see ya!",
      "Thought you'd never leave..",
      "Until we meet again..",
      "Take some of the empties with ya, eh?"
   ],
   get_random(M).

isare([_H]) ->
   " is ";
isare([_H|_T]) ->
   " are ".
concatenate_names([]) -> "";
concatenate_names([T]) -> T;
concatenate_names([H|T]) ->
   concatenate_names(H, T).
concatenate_names(String, [T]) ->
   lists:concat([String, " and ", T]);
concatenate_names(String, [H|T]) ->
   concatenate_names(lists:concat([String, ", ", H]), T).
open(Names) ->
   M = [
      ["The bike shop is open, with ", concatenate_names(Names), " on duty"],
      ["Yes, we're open! ", concatenate_names(Names), isare(Names), "here."],
      ["Open for business - ", concatenate_names(Names), isare(Names), "helping out."],
      [concatenate_names(Names), isare(Names), "running the show. Come on by!"],
      [concatenate_names(Names), isare(Names), "at the shop. Come on by!"]
   ],
   lists:flatten(get_random(M)).

closed() ->
   M = [
      "The bike shop is closed right now",
      "Sorry, we are not open right now",
      "Usually open from 6pm to 9pm on weekdays.. But, not right now, sorry!",
      "There is noone here - the bike shop is closed",
      "Sorry, we're not here right now.."
   ],
   get_random(M).
