-module(signup_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%
% contexts:
%
% 1- no one signed in
% 2- one or more volunteer signed in
%
% initial message from:
%
% 1- unknown
% 2- expired member
% 3- current monthly member
% 4- current annual member
% 5- volunteer
%
% recognizable messages:
%
% 1- signup
% 2- signup John Doe
% 3- signup month
% 4- signup month Jim Bo
% 5- signup year
% 6- signup year Sally Mae
%
%

% no one signed in / unknown
t100_signup_empty_unknown_test() ->
   Script = [signup, wait],
   Expected = [
      {"123", signup}, {{signup_closed_unknown}, "123"}
   ],
   Set = [
      ["123", unknown, Script]
   ],
   ?assertEqual(Expected, test:run(Set)).

% no one signed in / expired member
t101_signup_empty_expired_test() ->
   Script = [signup, wait],
   Expected = [
      {"112", signup}, {{signup_closed_expired}, "112"}
   ],
   Set = [
      ["112", member, Script]
   ],
   ?assertEqual(Expected, test:run(Set)).

% no one signed in / current member
t102_signup_empty_current_test() ->
   Script = [signup, wait],
   Expected = [
      {"113", signup}, {{signup_closed_current}, "113"}
   ],
   Set = [
      ["113", member, Script]
   ],
   ?assertEqual(Expected, test:run(Set)).

% no one signed in / volunteer
t103_signup_empty_volunteer_test() ->
   Script = [signup, wait],
   Expected = [
      {"111", signup}, {{signup_closed_volunteer}, "111"}
   ],
   Set = [
      ["111", volunteer, Script]
   ],
   ?assertEqual(Expected, test:run(Set)).

% volunteer present / unknown / deny
t200_signup_deny_unknown_test() ->
   Person = [delay, signup, delay],
   Volunteer = [in, wait, wait, deny, out, wait],
   Expected = [
      {"111", in}, {{hello}, "111"},
      {"123", signup}, {{signup_unknown, ["month"]}, "111"},
      {"111", deny},
      {"111", out}, {{bye}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer],
      ["123", unknown, Person]
   ],
   ?assertEqual(Expected, test:run(Set)).

t202_signup_approve_unknown_test() ->
   Person = [delay, signup, wait],
   Volunteer = [in, wait, wait, allow, out, wait],
   Expiry = "today + one month", % todo
   Expected = [
      {"111", in}, {{hello}, "111"},
      {"123", signup}, {{signup_unknown, ["month"]}, "111"},
      {"111", allow}, {{approved, Expiry}, "123"},
      {"111", out}, {{bye}, "111"}
   ],
   Set = [
      ["123", unknown, Person],
      ["111", volunteer, Volunteer]
   ],
   % db op to remove membership (leave no trace)
   ?assertEqual(Expected, test:run(Set)).

t202_signup_approve_month_unknown_test() ->
   Person = [delay, signup, wait],
   Volunteer = [in, wait, wait, allow, out, wait],
   Expiry = "today + one month", % todo
   Expected = [
      {"111", in}, {{hello}, "111"},
      {"123", {signup, ["month"]}}, {{signup_unknown, ["month"]}, "111"},
      {"111", allow}, {{approved, Expiry}, "123"},
      {"111", out}, {{bye}, "111"}
   ],
   Set = [
      ["123", unknown, Person],
      ["111", volunteer, Volunteer]
   ],
   % db op to remove membership (leave no trace)
   ?assertEqual(Expected, test:run(Set)).

t206_signup_approve_year_unknown_test() ->
   Person = [delay, {signup, ["year"]}, wait],
   Volunteer = [in, wait, wait, allow, out, wait],
   Expiry = "today + one year", %todo
   Expected = [
      {"111", in}, {{hello}, "111"},
      {"123", {signup, ["year"]}}, {{signup_unknown, ["year"]}, "111"},
      {"111", allow}, {{approved, Expiry}, "123"},
      {"111", out}, {{bye}, "111"}
   ],
   Set = [
      ["123", unknown, Person],
      ["111", volunteer, Volunteer]
   ],
   % db op to remove membership (leave no trace)
   ?assertEqual(Expected, test:run(Set)).

t208_signup_approve_month_name_unknown_test() ->
   Person = [delay, {signup, ["month", "John", "Doe"]}, wait],
   Volunteer = [in, wait, wait, allow, out, wait],
   Expiry = "today + one month",
   Expected = [
      {"111", in}, {{hello}, "111"},
      {"123", {signup, ["month", "John", "Doe"]}}, {{signup_unknown, ["month", "John", "Doe"]}, "111"},
      {"111", allow}, {{approved, Expiry}, "123"},
      {"111", out}, {{bye}, "111"}
   ],
   Set = [
      ["123", unknown, Person],
      ["111", volunteer, Volunteer]
   ],
   % db op to remove membership (leave no trace)
   ?assertEqual(Expected, test:run(Set)).

t210_verify_membership_valid_test() ->
   Person = [delay, signup, wait, verify, wait],
   Volunteer = [in, wait, wait, allow, out, wait],
   Expiry = "today + one month", % todo
   Expected = [
      {"111", in}, {{hello}, "111"},
      {"123", signup}, {{signup_unknown, ["month"]}, "111"},
      {"111", allow}, {{approved, Expiry}, "123"},
      {"111", out}, {{bye}, "111"},
      {"123", verify}, {{membership_valid, Expiry}, "123"}
   ],
   Set = [
      ["123", unknown, Person],
      ["111", volunteer, Volunteer]
   ],
   % TODO: db op to remove membership (leave no trace)
   ?assertEqual(Expected, test:run(Set)).

t212_verify_membership_expired_test() ->
   Person = [verify, wait],
   Expiry = "today + one month", % todo
   Expected = [
      {"123", verify}, {{membership_expired, Expiry}, "123"}
   ],
   Set = [
      ["123", unknown, Person]
   ],
   % db op to remove membership (leave no trace)
   ?assertEqual(Expected, test:run(Set)).

% Expected response per (context-source-message)
%
% a) x-3-[3,4]: (number, "Your membership expires on _____, no need to renew until then.")
% b) x-4-[3,4,5,6]: (number, "Your membership expires on _____, no need to renew until then.")
% c) 1-[1,2]-x: (number, "Please come to the shop during business hours to sign up.")
% d) 1-3-[5,6]: (number, "Please come to the shop during business hours to sign up.")
% e) 2-[1,2]-[1,2,3,4]: (volunteer, "(number) requesting monthly membership, respond (yes) if $5 paid, (no) otherwise."
% f) 2-[1,2]-[5,6]: (volunteer, "(number) requesting annual membership, respond (yes) if $20 paid, (no) otherwise."
% g) 2-[3]-[5,6]: (volunteer, "(number) requesting membership upgrade to annual, respond (yes) if $15 paid, (no) otherwise."
%
% In cases of f) and g), the transaction is incomplete until the volunteer responds, and the original sender receives a confirmation / denial message
%
% f1) volunteer responds (yes): "(number, Your new membership is valid until _____")
% f2) volunteer responds (no): [no response]
% g1) volunteer responds (yes): "(number, Your new membership is valid until _____")
% g2) volunteer responds (no): [no response]
%