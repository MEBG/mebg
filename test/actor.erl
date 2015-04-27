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
   % io:format("[actor] ~p is ~p (~p) doing ~p~n",
   %   [self(), Number, Role, Script]),
   relay ! {self(), Number, Role},
   consume(Script).

consume([]) ->
   % io:format("[actor] ~p finished~n", [self()]),
   relay ! {self(), remove};
consume([T]) -> process(T, []);
consume([H|T]) -> process(H, T).

process(delay, T) -> process({delay, 25}, T);
process({delay, Duration}, T) ->
   % io:format("[actor] ~p delaying ~pms~n", [self(), Duration]),
   timer:sleep(Duration),
   consume(T);

process(wait, T) ->
   % io:format("[actor] ~p waiting~n", [self()]),
   receive _ ->
      % io:format("[actor] ~p waited and got ~p~n", [self(), M]),
      consume(T)
   after 250 -> void
      % io:format("[actor] ~p timed out~n", [self()])
   end;

process(Message, T) ->
   % io:format("[actor] ~p: <~p>~n", [self(), Message]),
   relay ! {self(), Message},
   consume(T).
