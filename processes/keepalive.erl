% for launching registered processes that must keep running
-module(keepalive).
-export([launch/4]).

launch(Name, Module, Fun, Args) ->
   Pid = spawn(Module,Fun,Args),
   register(Name,Pid),
   spawn(fun() ->
      Ref = monitor(process, Pid),
      receive
         {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p (~p) died: ~p~n", [Pid,Name,Why]),
            io:format("restarting ~p~n", [Name]),
            launch(Name,Module,Fun,Args)
      end
   end).
