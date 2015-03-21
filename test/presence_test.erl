-module(presence_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%
% Test signing in and out
%
% sign in by unknown
% sign in by member
% sign in and out by volunteer
% double sign in by volunteer
% double sign out by volunteer
%



t100_signin_unknown_test() ->
   Person = [in, wait],
   Expected = [
      {"123", in},
      {{default}, "123"}
   ],
   Set = [
      ["123", unknown, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t102_signout_unknown_test() ->
   Person = [out, wait],
   Expected = [
      {"123", out},
      {{default}, "123"}
   ],
   Set = [
      ["123", unknown, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t104_signin_member_test() ->
   Person = [in, wait],
   Expected = [
      {"112", in},
      {{default}, "112"}
   ],
   Set = [
      ["112", member, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t106_signout_member_test() ->
   Person = [out, wait],
   Expected = [
      {"112", out},
      {{default}, "112"}
   ],
   Set = [
      ["112", member, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t110_signin_signout_volunteer_test() ->
   Person = [in, wait, out, wait],
   Expected = [
      {"111", in},
      {{hello}, "111"},
      {"111", out},
      {{bye}, "111"}
   ],
   Set = [
      ["111", volunteer, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t112_signin_twice_volunteer_test() ->
   Person = [in, wait, in, out, wait],
   Expected = [
      {"111", in},
      {{hello}, "111"},
      {"111", in},
      {"111", out},
      {{bye}, "111"}
   ],
   Set = [
      ["111", volunteer, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t114_signin_twice_volunteer_test() ->
   Person = [in, wait, out, wait, out],
   Expected = [
      {"111", in},
      {{hello}, "111"},
      {"111", out},
      {{bye}, "111"},
      {"111", out}
   ],
   Set = [
      ["111", volunteer, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

