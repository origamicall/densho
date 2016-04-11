-module(densho_prov).

-callback send_sms(densho:phone(), densho:message(), densho:src()) ->
    ok | {error, densho:reason()}.
