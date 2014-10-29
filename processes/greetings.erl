-module(greetings).
-export([
   hello_phrases/0, hello/0,
   bye_phrases/0, bye/0,
   open_phrases/1, open/1,
   closed_phrases/0, closed/0]).

get_random(List) ->
   [R|_] = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
   R.

hello_phrases() -> [
   "Welcome!",
   "Thought you'd never drop by!",
   "It's been a while, pardner",
   "Nice to see you",
   "Where have YOU been?",
   "Once more unto the breach",
   "Nice day!",
   "G'day mate",
   "A good day to wrench around..",
   "Uhh, sorry about the mess..",
   "Hi. I think we're out of chain breakers..",
   "Bienvenidos compadre!"
].
hello() ->
   get_random(hello_phrases()).

bye_phrases() -> [
   "So long!",
   "Thanks for bringin' it brah!",
   "And good riddance to you, sir!",
   "Til next time!",
   "Good times.. see ya!",
   "Sheesh, thought you'd never leave..",
   "Until we meet again..",
   "Take some of the empties with ya, eh?"
].
bye() ->
   get_random(bye_phrases()).

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
open_phrases(Names) -> [
   ["The bike shop is open, with ", concatenate_names(Names), " on duty"],
   ["Yes, we're open! ", concatenate_names(Names), isare(Names), "here."],
   ["Open for business - ", concatenate_names(Names), isare(Names), "helping out."],
   [concatenate_names(Names), isare(Names), "running the show. Come on by!"],
   [concatenate_names(Names), isare(Names), "at the shop. Come on by!"]
].
open(Names) ->
   lists:flatten(get_random(open_phrases(Names))).

closed_phrases() -> [
   "The bike shop is closed right now",
   "Sorry, we are not open right now",
   "Usually open from 6pm to 9pm on weekdays.. But, not right now, sorry!",
   "There is noone here - the bike shop is closed",
   "Sorry, we're not here right now.."
].
closed() ->
   get_random(closed_phrases()).
