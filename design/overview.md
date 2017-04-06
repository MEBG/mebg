# mebg #
Bike shop membership / orders / inventory management

* low-tech, accessible to everyone everywhere
* cheap and durable


## Overview ##
The main mode of interaction with the system is using text messages (via a new MEBG phone number), with a basic web app for admin, reports and general information.

The focus is on a minimum set of key actions:

* signing up members
* renewing memberships
* taking part orders

To support these, a few more actions are required:

* signing up volunteers
* volunteers signing in / out on their shift
* taking deposits from members
* notifying members of completed orders

Estimated cost is $1/mo for a sms-only phone number, plus $0.0075 per text message.


## Actions ##
Text messages sent to the MEBG phone number are interpreted as commands: action name followed by parameters. Each person interacting with the system is recognized by their phone number. To mirror the current structure, a person can be a Volunteer, a Member, or Unrecognized. Each class of user can invoke certain actions.

### Actions from Unrecognized persons ###
A person with no membership can:

* check if coop is open: `STATUS`
* request a membership: `SIGN UP [<month>|<year>] [<first>[<last name>]]`
* transfer existing paper membership: `TRANSFER <join date> [<month>|<year>] [<first>[<last name>]]`
* sign up as a volunteer: `VOLUNTEER <first><last name> <email@address>`

A membership request will be pending until an on-shift volunteer acknowledges receipt of cash.

A volunteer signup request is approved when all current volunteers respond with approval.

### Member actions ###
A person with a membership can also:

* check if their membership is still valid: `VERIFY`
* request a membership renewal: `RENEW [<month>|<year>]`
* order a part from Babac catalogue: `ORDER <sku>`
* make a deposit: `DEPOSIT <amount>`
* check their balance: `BALANCE`

Any renewal request or deposit request will be pending until an on-shift volunteer acknowledges receipt of cash.

### Volunteer actions ###
Actions available to volunteers fall into three categories:

* basic actions like order, deposit etc
* schedule-related actions
* responses to member requests

#### Basic actions ####
Volunteers can invoke most of the Member actions:

* deposit (does not require confirmation)
* check balance
* order part
* check status

Additionally, volunteers can:

* order supplies for the shop: `RESTOCK <sku>`
* receive cash from members `RECEIVE <amount>`

#### Schedule actions ####
A volunteer can notify the system of their presence at the shop by signing in and out. This allows the system to route member requests to volunteers which are present. The two actions are:

* begin shift: `ARRIVE`
* end shift: `DEPART`

The system tracks the schedules of all volunteers so that members may know if the shop will be open at a given time. To facilitate this, volunteers can:

* add a day to schedule: `MONDAY|TUESDAY|...|SUNDAY [<start time> <end time>]`
* remove a day from schedule: `NOT MONDAY|TUESDAY|...|SUNDAY`
* drop a shift: `DROP [<date>]`
* pick up a dropped shift: `PICKUP`

When a volunteer drops a shift, all other volunteers receive a notification. Responding with `PICKUP` indicates the intent to fill in for that shift.

Sometimes a volunteer may not be available for a long time (vacation, busy with other things). To manage extended absences, there are two actions:

* go away (no more messages / struck from schedule): `AWAY`
* return (reactivate all interactions): `BACK`

#### Response actions ####
On-shift volunteer(s) will receive membership / renewal / deposit requests via the system, one request per volunteer at a time. They can respond in the affirmative to acknowledge receipt of funds from the member, or in the negative to dismiss the request as unfulfilled. In the affirmative case, the member receives a text message that constitutes a receipt.

The action keywords are:

* confirm: `APPROVE`
* deny: `DENY`


## Reports ##
Reports are available via the web app. Since interactions with the web app can reveal potentially confidential information (as well as affect balances and part counts) authentication is required. 

To authenticate in browser:

* submit your phone number via login form
* system sends a text message with a numeric code
* submit the code on 2nd form
* browser session authenticated

Your device will remain authenticated (unless you clear cache etc) so this should be an infrequent interaction.

### Order list report ###
This report allows a volunteer to review the list of ordered parts, and check off specific parts as bought / delivered.
When parts are marked as delivered, the system will send notification texts to members who ordered these parts.

### Other reports ###
* memberships
* income from stock sold
* inventory

## General info ##
A landing page for non-authenticated visitors:

* logo, address, hours
* mission statement
* links to social media
