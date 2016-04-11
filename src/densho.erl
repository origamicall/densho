-module(densho).
-export([process_sms/2, process_sms/3]).
-include("densho.hrl").


process_sms(Dst, Message)->
    process_sms([], Dst, Message).

process_sms(TypeMessage, Dst, Message) ->
    Providers = densho_router:get_providers(),
    send_sms_providers(TypeMessage, Dst, Message, Providers).



send_sms_providers(_TypeMessage, _Dst, _Message, []) ->
    {error, "can't send sms to"};

send_sms_providers(TypeMessage, Dst, Message, [{Provider, Module}| _Tail]) ->
    lager:info("Process message to ~p provider:~p", [Dst, Provider]),
    Module:send_sms(TypeMessage, Dst, Message).
