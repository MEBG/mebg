%
% Relay is used to route messages between test actors and testable code
%
% Depends on [mode] of sender (sms process) - in test mode, sms routes
% messages via test_send, which uses test_relay registered process
%
% Skips receiver by messaging shop directly
%

-module(test).

-import('../src/keepalive.erl', [init/1]).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


% bootstrap test environment
init() ->
   keepalive:init(test),
   register(relay, spawn(test,relay,[#{}, []])).

run(Set) ->
   [spawn(actor, init, [Number, Role, Script])
   || [Number, Role, Script] <- Set],
   timer:sleep(100),
   relay ! {self(), transcribe},
   relay ! empty,
   receive Actual ->
      Actual
   after 150 ->
      false
   end.

kill() ->
   keepalive:stop(),
   unregister(relay).

% collects all received messages for later inspection
relay(Actors, Transcript) ->
   receive
      % relay message from test sender to actor
      {send, Number, Message} ->
         {Actor, _} = maps:get(Number, Actors),
         Actor ! Message,
         relay(Actors, [{Message, Number} | Transcript]);
      % add actor to active list
      {Pid, Number, Role} ->
         A1 = maps:put(Number, {Pid, Role}, Actors),
         A2 = maps:put(Pid, {Number, Role}, A1),
         relay(A2, Transcript);
      % relay message from actor to shop
      {Pid, {Action, Parameters}} ->
         {Number, Role} = maps:get(Pid, Actors),
         coop ! {{Pid, Number, Role, null, null, null}, Action, Parameters},
         relay(Actors, [{Number, {Action, Parameters}} | Transcript]);
      {Pid, remove} ->
         {Number, _} = maps:get(Pid, Actors),
         relay(maps:without([Number,Pid], Actors), Transcript);
      {Pid, transcribe} ->
         Pid ! lists:reverse(Transcript),
         relay(Actors, Transcript);
      {Pid, Action} ->
         {Number, Role} = maps:get(Pid, Actors),
         coop ! {{Pid, Number, Role, null, null, null}, Action, null},
         relay(Actors, [{Number, Action} | Transcript]);
      empty -> relay(#{}, [])
   end.
