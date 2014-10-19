-module(volunteer).
-export([loop/0]).


% loop here and receive messages
loop() ->
   receive
      % handle signup request
      {Pid, signup} ->
         % send outgoing SMS to this volunteer
         signup(Pid);
      % terminate process on "depart"
      goodbye ->
         void;
      Other ->
         io:format("unexpected message sent to volunteer: ~p~n", [Other]),
         loop()
   end.

% loop here until signup-related message is received
signup(Pid) ->
   receive
      approved ->
         Pid ! approved,
         loop();
      denied ->
         Pid ! denied,
         loop()
   after 600000 -> % 10 minute timeout
      Pid ! denied,
      io:format("(V): signup request timed out~n")
   end.
