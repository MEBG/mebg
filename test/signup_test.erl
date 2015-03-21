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

