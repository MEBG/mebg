#mebg#

Bike shop membership / orders / inventory management

* low-tech, accessible to everyone everywhere
* cheap and durable


##Overview##


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


##Actions##

Commands can be issued to the system by texting the action name (with parameters if required) to the MEBG phone number. Each person interacting with the system is recognized by their phone number. To mirror the current structure, a person can be a Volunteer, a Member, or Unrecognized. Each class of user can invoke certain actions.

###Actions from Unrecognized persons###

A person with no membership can:

* request a membership: `SIGN UP [<month>|<year>] [<first>[<last name>]]`
* check if coop is open: `STATUS`

A membership request will be pending until an on-shift volunteer acknowledges receipt of cash.

###Member actions###

A person with a membership can:

* check if their membership is still valid: `VERIFY`
* request a membership renewal: `RENEW [<month>|<year>]`
* order a part from Babac catalogue: `ORDER <sku>`
* make a deposit: `DEPOSIT <amount>`
* check their balance: `BALANCE`

Any renewal request or deposit request will be pending until an on-shift volunteer acknowledges receipt of cash.

###Volunteer actions###

Volunteers have two kinds of actions available to them:

* simple actions like signing in / out, and all of above Member actions
* member-related actions (responses to requests made by Members)

####Simple actions####

A volunteer can notify the system of their presence at the shop by signing in and out. This allows the system to route member requests to volunteers which are present. Volunteer signups are validated by admin.

* sign up as a volunteer: `VOLUNTEER <first><last name> <email@address>`
* begin shift: `[OPEN|BEGIN|IN]`
* end shift: `[CLOSE|END|OUT]`

####Response actions####

On-shift volunteer(s) will receive membership / renewal / deposit requests via the system, one at a time. They can respond in the affirmative (`TRUE|YES|OK|ACCEPT`) to acknowledge receipt of funds from the member, or in the negative (`FALSE|NO|CANCEL|DENY`) to dismiss the request as unfulfilled. In the affirmative case, the member receives a text message that constitutes a receipt.


##Reports##

Reports are available via the web app. Since interactions with the web app can reveal potentially confidential information (as well as affect balances and part counts) authentication is required. 

To authenticate:

* submit your phone number via login form
* system sends a text message with a numeric code
* submit the code on 2nd form
* browser session authenticated

Your computer / device will remain authenticated (unless you clear cache etc) so this should be an infrequent interaction.

###Order list report###

This report allows a volunteer to review the list of ordered parts, and check off specific parts as bought / delivered.
When parts are marked as delivered, the system will send notification texts to members who ordered these parts.

###Other reports###

* memberships
* income from stock sold
* inventory

##General info##

A landing page for non-authenticated visitors:

* logo, address, hours
* mission statement
* links to social media