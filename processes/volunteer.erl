-module(volunteer).
-export([loop/0]).


% loop here and receive messages
loop() ->
   receive
      % handle signup request
      {Pid, signup, Arguments} ->
         % send outgoing SMS to this volunteer
         io:format("~p got signup request (~p)~n", [self(),Arguments]),
         signup(Pid);
      % terminate process on "depart"
      goodbye ->
         io:format("terminating ~p~n", [self()]),
         void;
      Other ->
         io:format("some message sent to volunteer: ~p~n", [Other]),
         loop()
   end.

% loop here until signup-related message is received
signup(Pid) ->
   receive
      approved ->
         io:format("waited for: approved~n"),
         Pid ! approved,
         loop();
      denied ->
         io:format("waited for: denied~n"),
         Pid ! denied,
         loop()
   after 600000 -> % 10 minute timeout
      Pid ! denied,
      io:format("signup request timed out~n")
   end.
