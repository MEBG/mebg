% process dispatching outgoing sms

-module(sender).
-export([loop/1]).

loop(Mode) ->
   receive
      {mode, test} -> loop(test);
      {mode, normal} -> loop(normal);
      {send, Number, Message} when normal == Mode ->
         send(Number,Message),
         loop(Mode);
      {send, Number, Message} when test == Mode ->
         test_send(Number,Message),
         loop(Mode)
   end.


send(Number, Message) ->
   {Username, Password} = secrets:twilio_auth(),
   UrlBase = lists:concat([
      "api.twilio.com/2010-04-01/Accounts/",
      Username,"/Messages.json"]),
   Url = ["https://", Username, ":", Password, "@", UrlBase],
   Parameters = [
      "From=",secrets:twilio_number(),
      "&To=",Number,
      "&Body=", Message],
   io:format("sending to ~p: ~p~n", [Number,Message]),
   % fail hard on any error
   {ok,{{"HTTP/1.1",201,"CREATED"},_,_}} = httpc:request(post, 
       {lists:flatten(Url), [], 
       "application/x-www-form-urlencoded",
       lists:flatten(Parameters)
       }, [], []).

test_send(Number, Message) ->
   io:format("SMS to be sent to ~p: ~p~n", [Number,Message]),
   test_relay!{send,Number,Message}.

