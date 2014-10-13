% this process spawns new processes corresponding to received sms
-module(receiver).
-export([loop/0]).

loop() ->
   Volunteers = ["111","222"],
   Members = ["123","234","345"],
   receive
      {From, Body} ->
         IsVolunteer = string:str(Volunteers, [From]) > 0,
         IsMember = string:str(Members, [From]) > 0,
         Tokens = string:tokens(Body, " "),
         [Action|Arguments] = Tokens,
         if
            IsVolunteer ->
               io:format("~p (volunteer) sent ~p~n", [From,Body]),
               P = spawn(volunteer, loop, []);
            IsMember ->
               io:format("~p (member) sent ~p~n", [From,Body]),
               P = spawn(member, loop, []);
            true ->
               io:format("~p (unknown) sent ~p~n", [From,Body]),
               P = spawn(unknown, loop, [])
         end,
         P ! {From, Action, Arguments},
         loop()
   end.