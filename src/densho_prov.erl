-module(densho_prov).


-type phone() :: binary().
-type message() :: binary().
-type reason() :: atom() | term().

-callback send_sms(phone(), message()) -> ok | {error, reason()}.

