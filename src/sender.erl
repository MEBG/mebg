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
   timer:sleep(1000), % throttle outgoing messages
   Response = httpc:request(post, 
       {lists:flatten(Url), [], 
       "application/x-www-form-urlencoded",
       lists:flatten(Parameters)
       }, [], []),
   % fail hard on any error
   {ok,{{"HTTP/1.1",201,"CREATED"},_,_}} = Response.

test_send(Number, Message) ->
  io:format("SMS to be sent to ~p: ~p~n", [Number,Message]),
  case lists:member(test_relay, registered()) of
    true ->
      test_relay!{send,Number,Message};
    false ->
      void
  end.

