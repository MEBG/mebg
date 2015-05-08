%
% Produce messages send by the system from templates and parameters
%

-module(messages).

-export([render/1, cc/2]).

% English by default
render({Template}) -> render("en", Template, null);
render({Template, Parameters}) -> render("en", Template, Parameters).

% retrieve message templates from file and
% produce a message according to arguments
render(Language, Template, Parameters) ->
   File = lists:concat(["i18n/message_strings_", Language, ".erl"]),
   {ok, L} = file:consult(File),
   M = maps:from_list(L),
   {Op, Elems} = maps:get(Template, M),
   assemble(Op, Elems, Parameters).

% return a random item from a list
get_random_item(List) ->
   [R|_] = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
   R.

% concatenate list with appropriate separators
cc(_, []) -> "";
cc(Conj, [H|T]) -> cc(Conj, H, T).
cc(Conj, String, [T]) -> lists:concat([String, Conj, T]);
cc(Conj, String, [H|T]) -> cc(Conj, lists:concat([String, ", ", H]), T).

% build a complex phrase
assemble(random, {build, {[Singular, _], _}, Phrases}, [H]) ->
   P1 = lists:keyreplace(verb, 1, get_random_item(Phrases), {Singular}),
   P2 = lists:keyreplace(subject, 1, P1, {H}),
   lists:concat([X || {X} <- P2]);
assemble(random, {build, {[_, Plural], Conjunction}, Phrases}, [H|T]) ->
   assemble(random, {build, {[Plural, null], null}, Phrases}, [cc(Conjunction, [H|T])]);

assemble(random, {multi, Phrases}, Subs) ->
   assemble(multi, get_random_item(Phrases), Subs);
assemble(multi, Phrase, Subs) ->
   multiple(Subs, Phrase);

assemble(random, Elements, _) -> get_random_item(Elements);
assemble(concatenate, Elements, _) -> lists:flatten(Elements).

% format a phrase substituting multiple items
multiple([], []) -> "";
multiple([], [sub|Phrase]) -> multiple([], Phrase);
multiple([], Phrase) -> lists:concat(Phrase);
multiple([_], []) -> "";
multiple([Sub], [sub|Phrase]) ->
   lists:concat([Sub, "", multiple([], Phrase)]);
multiple([Hs|Subs], [sub|Phrase]) ->
   lists:concat([Hs, "", multiple(Subs, Phrase)]);
multiple([Hs|Subs], [P|Phrase]) ->
   lists:concat([P, "", multiple([Hs|Subs], Phrase)]);
multiple([Sub], [P|Phrase]) ->
   lists:concat([P, "", multiple([Sub], Phrase)]);
multiple([Hs|_], [Phrase]) ->
   lists:concat([Hs, "", Phrase]);
multiple([Sub], [Phrase]) ->
   lists:concat([Sub, "", Phrase]).
