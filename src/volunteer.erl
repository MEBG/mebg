-module(volunteer).
-export([loop/1]).


% loop here and receive messages
loop(Number) ->
   receive
      % handle signup request
      {Pid, signup} ->
         % send outgoing SMS to this volunteer
         sms ! {send, Number,"A membership request appears"},
         signup(Number, Pid);
      % terminate process on "depart"
      goodbye -> void;
      Other ->
         io:format("unexpected message sent to volunteer: ~p~n", [Other]),
         loop(Number)
   after 900000 -> % check every 15 minutes
      {_,{Hour,_,_}} = erlang:localtime(),
      if
         Hour < 17 orelse Hour > 21 -> % magic numbers, bad form
            sms ! {send, Number, {timeout}},
            Person = {void, Number, volunteer, void, void, void},
            coop ! {Person, depart, void};
         true ->
            loop(Number)
      end
   end.

% loop here until signup-related message is received
signup(Number, Pid) ->
   receive
      approved ->
         Pid ! approved,
         loop(Number);
      denied ->
         Pid ! denied,
         loop(Number)
   after 300000 -> % 5 minute timeout
      Pid ! denied,
      io:format("(V): signup request timed out~n"),
      loop(Number)
   end.
