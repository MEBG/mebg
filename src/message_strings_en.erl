{default, {concatenate, [
   "mile end bike garage",
   "135 rue Van Horne #201",
   "txt 'H' for schedule, 'S' to check if open.",
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
         ]
      }
   }
}.
