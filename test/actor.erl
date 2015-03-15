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

-export([init/2]).

init({Number, Role}, Script) ->
   relay ! {self(), Number, Role},
   consume(Script).

consume([]) ->
   relay ! {self(), remove};

consume([T]) ->
   process(T, []);

consume([H|T]) ->
   process(H, T).

process({send, Message}, T) ->
   relay ! {self(), Message},
   consume(T);

process({wait, Expected}, T) ->
   receive
      Actual ->
         Actual = Expected, % pass if ok
         consume(T)
   end.