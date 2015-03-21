-module(misc_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%
% Test for miscellaneous features that do not fall into any larger category
%
%
% ** locked out **
%
% - recognizable strings: ["door","locked"]
%
% contexts:
% 1 - no one signed in
%
% send a response: (number, "Sorry, no one is here right now..")
%
%
% 2 - one or more volunteers signed in
%
% send two responses:
%  (number, "Not again! Hang tight, someone will be down in a minute.")
%  (volunteer, "Someone is locked out downstairs!")
%
% if more than one volunteer is signed in,
% all volunteers receive the response
%

% no one signed in
t100_locked_empty_test() ->
   Script = [door, wait],
   Expected = [
      {"123", door},
      {{door_closed}, "123"}
   ],
   Set = [["123", unknown, Script]],
   ?assertEqual(Expected, test:run(Set)).

t110_signin_test() ->
   Script = [in, wait, out, wait],
   Set = [["111", volunteer, Script]],
   Expected = [
      {"111", in},
      {{hello}, "111"},
      {"111", out},
      {{bye}, "111"}
   ],
   ?assertEqual(Expected, test:run(Set)).

t120_locked_present_test() ->
   Person = [delay, door, wait],
   Volunteer = [in, wait, wait, out, wait],
   Expected = [
      {"111", in},
      {{hello}, "111"},
      {"123", door},
      {{door_open}, "111"},
      {{door_wait}, "123"},
      {"111", out},
      {{bye}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer],
      ["123", unknown, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).
