% functionality to dispatch outgoing sms

-module(sender).
-export([send/2,sms/2]).

sms(Number, Message) ->
   io:format("SMS to be sent to ~p: ~p~n", [Number,Message]).

send(Number, Message) ->
   {Username, Password} = secrets:twilio_auth(),
   UrlBase = "api.twilio.com/2010-04-01/Accounts/ACa66dd367e3772ec8696f298dd7e528a5/Messages.json",
   Url = ["https://", Username, ":", Password, "@", UrlBase],
   Parameters = ["From=",secrets:twilio_number(),"&To=",Number, "&Body=", Message],
   httpc:request(post, 
       {lists:flatten(Url), [], 
       "application/x-www-form-urlencoded",
       lists:flatten(Parameters)
       }, [], []),
   io:format("SMS to ~p sent: ~p~n", [Number,Message]).