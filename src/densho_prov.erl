-module(densho_prov).

-callback send_sms(densho:phone(), densho:message()) ->
    ok | {error, densho:reason()}.
