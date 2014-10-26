% this process parses incoming sms and
% forwards the message to main process
-module(receiver).
-export([loop/0]).

loop() ->
   receive
      {[43|Number], Body} -> % % number must be preceeded by a "+"
         % lowercase + tokenize body of message
         Tokens = string:tokens(string:to_lower(Body), " "),
         [A|Arguments] = Tokens,
         Action = list_to_atom(A),
         % fetch person data if known
         Person = db:get_person(Number),
         io:format("~p (~p) sent ~p~n", [Number,Person,Tokens]),
         % forward parsed message to shop process
         coop ! {Person, Action, Arguments},
         loop()
   end.
