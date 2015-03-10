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

-export([init/2, consume/1]).

% {Id, Number, Role, Name, Expiry, Balance}
init({_, Number, Role, Name, _, _}, Script) ->
   relay ! {{self(), Role, Name}, register, Number},
   consume(Script).

consume([]) -> void;

consume([T]) ->
   process(T);

consume([H|T]) ->
   process(H),
   consume(T).

process({send, Message}) ->
   io:format("[send] ~p~n", [Message]),
   relay ! {self(), Message};

process({wait, Expected}) ->
   receive
      Actual ->
         io:format("[received] ~p~n", [Actual]),
         Actual = Expected % pass if ok
   end.