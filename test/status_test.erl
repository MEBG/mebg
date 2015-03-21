-module(status_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%
% Test status message
%
% status message with no volunteers
% status message with one volunteer
% status message with multiple volunteers
%

t100_status_empty_test() ->
   Person = [status, wait],
   Expected = [
      {"123", status},
      {{shut}, "123"}
   ],
   Set = [
      ["123", unknown, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t110_status_open_test() ->
   Volunteer = [in, wait, delay, delay, out, wait],
   Person = [delay, status, wait],
   Expected = [
      {"111", in},
      {{hello}, "111"},
      {"123", status},
      {{open, [null]}, "123"},
      {"111", out},
      {{bye}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer],
      ["123", unknown, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).
