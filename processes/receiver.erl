% this process parses incoming sms and
% forwards the message to main process
-module(receiver).
-export([loop/0]).

loop() ->
   receive
      {Number, Body} ->
         % lowercase + tokenize body of message
         Tokens = string:tokens(string:to_lower(Body), " "),
         [A|Arguments] = Tokens,
         Action = list_to_atom(A),
         Role = db:get_role(Number),
         % forward parsed message to shop process
         coop ! {Number, Role, Action, Arguments},
         loop()
   end.
