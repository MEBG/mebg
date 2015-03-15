% Process dispatching outgoing messages
%
% Can be switched between normal and test operation
%

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

% send message via twilio api
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

% send message via test relay (if registered, stdio otherwise)
test_send(Number, Message) ->
  case lists:member(relay, registered()) of
    true ->
      relay ! {send, Number, Message};
    false ->
      io:format("test sms NOT sent to ~p: ~p~n", [Number,Message])
  end.

