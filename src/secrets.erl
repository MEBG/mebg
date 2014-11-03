-module(secrets).
-export([twilio_auth/0, twilio_number/0]).

twilio_auth() -> {"username","password"}.
twilio_number() -> "18885551234".
admin_number() -> "18008008080".