% for launching registered processes that must keep running
-module(keepalive).
-export([launch/4, init/1]).


% spawn an auto-restarting process
launch(Name, Module, Fun, Args) ->
   Pid = spawn(Module,Fun,Args()),
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

% launch system
init(Mode) ->
   ssl:start(),
   inets:start(),
   launch(sms,sender,loop,fun() -> [Mode] end),
   % launch(box,cashbox,loop,fun() -> [db:get_transaction_balance()] end),
   launch(coop,shop,loop,fun() -> [db:get_present_volunteers()] end),
   launch(rcvr,receiver,loop, fun() -> [] end).
