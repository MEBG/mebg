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

         Role = get_role(Number),
         io:format("role: ~p~n",[Role]),
         io:format("action: ~p~n",[Action]),

         % forward parsed message to shop process
         coop ! {Number, Role, Action, Arguments},
         loop()
   end.

% retrieve role of person associated with given number (if known)
get_role(Number) ->
   sqlite3:open(main),
   [{columns, _}, {rows, Rows}] = sqlite3:read(main, person, {phone, Number}),
   if
      [] =/= Rows ->
         [{_,_,Role,_,_,_}] = Rows,
         list_to_atom(binary_to_list(Role));
      true ->
         unknown
   end.