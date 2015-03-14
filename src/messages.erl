%
% strings comprising various messages sent by system
%

-module(messages).

-export([render/2, render/3]).

render(Template, Message) -> render("en", Template, Message).

render(Language, Template, Parameters) ->
   File = lists:concat(["message_strings_", Language, ".erl"]),
   {ok, L} = file:consult(File),
   M = maps:from_list(L),
   {Op, Elems} = maps:get(Template, M),
   assemble(Op, Elems, Parameters).

% return a random item from a list
get_random(List) ->
   [R|_] = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
   R.

% concatenate list with appropriate separators
cc(Conj, [H|T]) -> cc(Conj, H, T).
cc(Conj, String, [T]) -> lists:concat([String, Conj, T]);
cc(Conj, String, [H|T]) -> cc(Conj, lists:concat([String, ", ", H]), T).


assemble(random, {build, {[Singular, _], _}, Phrases}, [H]) ->
   P1 = lists:keyreplace(verb, 1, get_random(Phrases), {Singular}),
   P2 = lists:keyreplace(subject, 1, P1, {H}),
   lists:concat([X || {X} <- P2]);
assemble(random, {build, {[_, Plural], Conjunction}, Phrases}, [H|T]) ->
   assemble(random, {build, {[Plural, null], null}, Phrases}, [cc(Conjunction, [H|T])]);

assemble(random, Elements, _) -> get_random(Elements);
assemble(concatenate, Elements, _) -> lists:concat(Elements).

