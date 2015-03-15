%
% Relay is used to route messages between test actors and testable code
%
% Depends on [mode] of sender (sms process) - in test mode, sms routes
% messages via test_send, which uses test_relay registered process
%
% Skips receiver by messaging shop directly
%

-module(test).

-compile(export_all).

% -include_lib("eunit/include/eunit.hrl").


% bootstrap test environment
init() ->
   keepalive:init(test),
   register(relay, spawn(test,relay,[#{}])).

% collects all received messages for later inspection
relay(Actors) ->
   receive
      % relay message from test sender to actor
      {send, Number, Message} ->
         {Actor, _} = maps:get(Number, Actors),
         Actor ! Message,
         relay(Actors);
      % add actor to active list
      {Pid, Number, Role} ->
         A1 = maps:put(Number, {Pid, Role}, Actors),
         relay(maps:put(Pid, {Number, Role}, A1));
      % relay message from actor to shop
      {Pid, {Action, Parameters}} ->
         {Number, Role} = maps:get(Pid, Actors),
         coop ! {Pid, Number, Role, Action, Parameters},
         relay(Actors);
      {Pid, remove} ->
         {Number, _} = maps:get(Pid, Actors),
         relay(maps:without([Number,Pid], Actors));
      quit -> void
   end.
