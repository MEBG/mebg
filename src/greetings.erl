-module(greetings).
-export([
   concatenate/1,
   hello_phrases/0, hello/0,
   bye_phrases/0, bye/0,
   open_phrases/1, open/1,
   closed_phrases/1, closed/1,
   late_phrases/0, late/0,
   shut_phrases/0, shut/0]).

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
concatenate([]) -> "";
concatenate([T]) -> T;
concatenate([H|T]) ->
   concatenate(H, T).
concatenate(String, [T]) ->
   lists:concat([String, " and ", T]);
concatenate(String, [H|T]) ->
   concatenate(lists:concat([String, ", ", H]), T).
open_phrases(Names) -> [
   ["The bike shop is open, with ", concatenate(Names), " on duty"],
   ["Yes, we're open! ", concatenate(Names), isare(Names), "here."],
   ["Open for business - ", concatenate(Names), isare(Names), "helping out."],
   [concatenate(Names), isare(Names), "running the show. Come on by!"],
   [concatenate(Names), isare(Names), "at the shop. Come on by!"]
].
open(Names) ->
   lists:flatten(get_random(open_phrases(Names))).

shut_phrases() -> [
   "The bike shop will not be open today.",
   "Sorry, today we are closed - txt SCHEDULE for business hours.",
   "Closed today - txt SCHEDULE to see when we're open."
].
shut() ->
   get_random(shut_phrases()).

closed_phrases(Scheduled) -> [
   ["The bike shop is closed right now. We're open from 6pm til 9pm today, with ",
   Scheduled, " volunteering."],
   ["Sorry, we are not open right now. ",
   Scheduled, " should be here between 6pm and 9pm)"],
   ["There is no one here - the bike shop is closed. Try between 18h and 21h, ",
   Scheduled, " will be there."],
   ["Sorry, we're not here right now.. ", Scheduled, " will open shop around 6pm."]
].
closed(Scheduled) ->
   lists:flatten(get_random(closed_phrases(Scheduled))).

late_phrases() -> [
   "Usually open from 6pm to 9pm on weekdays.. But, not right now, sorry!",
   "The bike coop is supposed to be open right now, but no one is here yet. Our apologies..",
   "Sorry, we should be open but no volunteers showed up yet.."
].
late() ->
   get_random(late_phrases()).
