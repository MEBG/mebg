-module(test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


trap(Timeout) ->
   test_listener ! self(),
   receive
      ok -> ok;
      error -> error
   after Timeout ->
      error
   end.


send(Number, Message) ->
   test_listener ! {Number, Message}.

status_test() ->
   register(test_listener, spawn(test,listener,[200])),
   rcvr!{"+15145535858","status"},
   timer:sleep(50),
   ok = trap(300).

listener(T) ->
   receive
      _ -> pass_mode()
      after T ->
         fail_mode()
   end.

pass_mode() ->
   io:format("listener in pass mode~n"),
   receive
      % {Number, _} ->
      %    io:format("pass mode discarding message from ~p~n", [Number]),
      %    pass_mode();
      Pid ->
         io:format("trap pass: ~p~n", [Pid]), 
         Pid ! ok
   end.

fail_mode() ->
   io:format("listener in fail mode~n"),
   receive
      % {Number, _} ->
      %    io:format("fail mode discarding message from ~p~n", [Number]), 
      %    fail_mode();
      Pid ->
         io:format("trap fail: ~p~n", [Pid]), 
         Pid ! error
   end.