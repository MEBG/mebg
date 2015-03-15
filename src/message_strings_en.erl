%
% strings comprising various messages sent by system
%

{default, {concatenate, [
   "mile end bike garage",10,
   "135 rue Van Horne #201",10,
   "txt 'H' for schedule, 'S' to check if open.",10,
   "http://bikegarage.org"
]}}.

{hello, {random, [
   "Welcome!",
   "Thought you'd never drop by!",
   "It's been a while, pardner",
   "Nice to see you",
   "Where have YOU been?",
   "Nice day!",
   "G'day mate",
   "A good day to wrench around..",
   "Uhh, sorry about the mess..",
   "Hi. I think we're out of chain breakers..",
   "Bienvenidos compadre!"
]}}.

{bye, {random, [
   "So long!",
   "Thanks for bringin' it brah!",
   "And good riddance to you, sir!",
   "Til next time!",
   "Good times.. see ya!",
   "Sheesh, thought you'd never leave..",
   "Until we meet again..",
   "Take some of the empties with ya, eh?"
]}}.

{timeout, {random, [
   "Schedule bot signed you out",
   "It's a little latte, imma call it a night",
   "Probly a bit past closing time, gonna sign you out now"
]}}.

{empty, {random, [
   "The schedule is empty, no one has signed up. Shop's closed.",
   "Looks like no one is signed up for a shift. That's sad..",
   "Nobody is volunteering these days, we're closed",
   "Bike shop is closed until someone signs up for a shift"
]}}.

{schedule,
   {random,
      {build, {[null, null], ""}, [
         [{"18h-21h "}, {subject}]
      ]}
   }
}.

{days,
   {random,
      {build, {[null, null], " and "}, [
      [{"You're signed up for "}, {subject}],
      [{"We've got you on "}, {subject}],
      [{"It says here you're doing "}, {subject}]
]}}}.

{notsignedup, {random, [
   "You're not signed up for any shifts.",
   "You are not signed up for any days of the week"
]}}.

{late, {random, [
   "Usually open from 6pm to 9pm.. But, not right now, sorry!",
   "The bike coop is supposed to be open right now, but no one is here yet. Our apologies..",
   "Sorry, we should be open but no volunteers showed up yet.."
]}}.

{shut, {random, [
   "The bike shop is not open today.",
   "Sorry, today we are closed - txt 'hours' for business hours.",
   "Closed today - txt 'hours' to see when we're open."
]}}.

{open,
   {random,
      {build, {[" is ", " are "], " and "}, [
         [{"The bike shop is open, with "}, {subject}, {" on duty"}],
         [{"Yes, we're open! "}, {subject}, {verb}, {"here."}],
         [{"Open for business - "}, {subject}, {verb}, {"helping out."}],
         [{subject}, {verb}, {"running the show. Come on by!"}],
         [{subject}, {verb}, {"at the shop. Come on by!"}]
      ]}
   }
}.

{door_open,
   {random, [
      "Downstairs door is locked again..",
      "Someone's locked out downstairs"
]}}.

{door_closed,
   {random, [
      "Sorry, no one is here right now.",
      "The bike shop is closed."
]}}.

{door_wait,
   {random, [
      "Not again! Nofified a volunteer, someone should be down in a minute.",
      "Someone's coming to open the door, as soon as they get this text."
]}}.
