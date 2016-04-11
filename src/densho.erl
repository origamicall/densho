-module(densho).
-export([process_sms/2, process_sms/3]).
-include("densho.hrl").

-type phone() :: binary().
-type message() :: binary().
-type reason() :: atom() | term().
-type encoding() :: utf8 | utf16.

-type provider() :: string() | atom().
-type route() :: {provider(), module()}.
-type routes() :: [route()].

-export_type([phone/0, message/0, reason/0, encoding/0, routes/0, provider/0]).

-spec process_sms(phone(), message()) -> ok | {error, reason()}.

process_sms(Dst, Message)->
    process_sms(utf8, Dst, Message).

-spec process_sms(encoding(), phone(), message()) -> ok | {error, reason()}.

process_sms(TypeMessage, Dst, Message) ->
    Providers = densho_router:get_providers(),
    send_sms_providers(TypeMessage, Dst, Message, Providers).

-spec send_sms_providers(encoding(), phone(), message(), routes()) ->
    ok | {error, reason()}.

send_sms_providers(_TypeMessage, _Dst, _Message, []) ->
    {error, "can't send sms to"};

send_sms_providers(TypeMessage, Dst, Message, [{Provider, Module}| _Tail]) ->
    lager:info("Process message to ~p provider:~p", [Dst, Provider]),
    Module:send_sms(TypeMessage, Dst, Message).
