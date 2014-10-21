% for launching registered processes that must keep running
-module(keepalive).
-export([launch/4, launch/0]).

launch(Name, Module, Fun, Args) ->
   Pid = spawn(Module,Fun,Args),
   register(Name,Pid),
   spawn(fun() ->
      Ref = monitor(process, Pid),
      receive
         {'DOWN', Ref, process, Pid, Why} ->
            Msg = lists:concat(["Process '",Name, "' crashed."]),
            sender:send(secrets:admin_number(), Msg),
            io:format("~p (~p) died: ~p~n", [Pid,Name,Why]),
            io:format("restarting ~p~n", [Name]),
            launch(Name,Module,Fun,Args)
      end
   end).

% launch all registered processes
launch() ->
   launch(box,cashbox,loop,[0]),
   launch(coop,shop,loop,[#{}]),
   launch(rcvr,receiver,loop,[]).