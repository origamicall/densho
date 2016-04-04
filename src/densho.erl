-module(densho).
-export([process_sms/2]).
-include("densho.hrl").


process_sms(Dst, Message)->
    Providers = densho_router:get_providers(),
    send_sms_providers(Dst, Message, Providers).

send_sms_providers(_Dst, _Message, []) ->
    {error, "can't send sms to"};


send_sms_providers(Dst, Message, [{Provider, Module}| _Tail]) ->
    lager:info("Process message to ~p provider:~p", [Dst, Provider]),
    Module:send_sms(Dst, Message).
