%
% Actor process - can be given scripts to interact with relay
%
% The actor consumes the script until it runs out, then terminates
%
% Messages are exchanged between relay and actor only (no actor-actor)
%
% Success is indicated by empty script after some default timeout
%
% Actor initialized with a phone number to match role
%


-module(actor).

-export([init/3]).

-include_lib("eunit/include/eunit.hrl").


init(Number, Role, Script) ->
   % io:format("~p spawned from ~p~n", [self(), Number]),
   relay ! {self(), Number, Role},
   consume(Script).

consume([]) -> relay ! {self(), remove};
consume([T]) -> process(T, []);
consume([H|T]) -> process(H, T).

process(delay, T) ->
   % io:format("~p delaying~n", [self()]),
   timer:sleep(50),
   consume(T);

process(wait, T) ->
   % io:format("~p waiting~n", [self()]),
   receive _ -> consume(T)
   after 2000 -> void
   end;

process(Message, T) ->
   relay ! {self(), Message},
   consume(T).
