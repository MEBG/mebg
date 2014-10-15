% this process parses incoming sms and
% forwards the message to main process
-module(receiver).
-export([loop/0]).

loop() ->
   Volunteers = ["111","222"],
   Members = ["123","234","345"],
   receive
      {Number, Body} ->
         % lowercase + tokenize body of message
         Tokens = string:tokens(string:to_lower(Body), " "),
         [Action|Arguments] = Tokens,
         % awkward, will be replaced with db lookup
         IsVolunteer = string:str(Volunteers, [Number]) > 0,
         IsMember = string:str(Members, [Number]) > 0,
         if
            IsVolunteer ->
               io:format("volunteer~n"),
               Role = volunteer;
            IsMember ->
               io:format("member~n"),
               Role = member;
            true ->
               io:format("not known~n"),
               Role = unknown
         end, % ..of awkward section
         % forward parsed message to shop process
         coop ! {Number, Role, Action, Arguments},
         loop()
   end.