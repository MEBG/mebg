% functionality to dispatch outgoing sms

-module(sender).
-export([send/2]).

send(Number, Message) ->
   io:format("SMS sent to ~p: ~p~n", [Number,Message]).