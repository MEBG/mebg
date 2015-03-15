% this process parses incoming sms and
% forwards the message to main process
-module(receiver).
-export([loop/0]).

loop() ->
   receive
      % parse incoming message, identify source and dispatch to shop process
      {[43|Number], Body} -> % number must be preceeded by a "+"
         % lowercase + tokenize body of message
         Tokens = string:tokens(string:to_lower(Body), " "),
         [A|Arguments] = Tokens,
         Action = list_to_atom(A),
         io:format("[~p] ~p received from ~p~n",
            [erlang:localtime(), Tokens, Number]),
         % fetch person data if known
         Person = db:get_person(Number),
         % forward parsed message to shop process
         coop ! {Person, Action, Arguments},
         loop();
      Other ->
         io:format("unrecognized: ~p~n", [Other]),
         loop()
   end.
