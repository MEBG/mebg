-module(test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% Test shim, allows for testing synchronous messages
% Used to test request-response pairs (eg, when "status" request is sent,
% we expect the system will respond with a message, and want to capture
% that message to compare expected vs actual response)
% Depends on [mode] of sender (sms process) - in test mode, sms routes
% messages via test_send, which uses test_relay registered process

% bootstrap test environment
init() ->
   keepalive:init(test),
   register(relay, spawn(test,relay,[#{}])).

% collects all received messages for later inspection
relay(Actors) ->
   receive
      {{Pid, Role, Name}, register, Number} -> % add actor to active list
         relay(maps:put(Number, {Pid, Role, Name}, Actors));
      {send, Number, Message} -> % fwd from test_send
         io:format("[## relay] ~p~n", [Message]),
         case maps:is_key(Number, Actors) of
            true ->
               {Actor, _, _} = maps:get(Number, Actors),
               Actor ! Message
         end,
         relay(Actors);
      {_, Message} ->
         io:format("[>> relay] ~p~n", [Message]),
         % Number = reverse-lookup-from-pid
         % send(Number, Message)
         relay(Actors);
      list_actors ->
         io:format("[actor count] ~p~n", [maps:size(Actors)]),
         relay(Actors);
      _ ->
         io:format("[>> relay] UNKNOWN~n"),
         relay(Actors)
   end.


% sends synchronous request to Number, returns response message
% send(Number, Message) ->
%    rcvr!{lists:flatten(["+"|Number]), Message}.
