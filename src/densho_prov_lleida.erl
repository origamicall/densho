-module(densho_prov_lleida).
-behaviour(densho_prov).

-export([send_sms/3, send_sms/4]).

-include_lib("xmerl/include/xmerl.hrl").
-include("lleida.hrl").

-define(HEADER, #{<<"content-type">> => <<"application/x-www-form-urlencoded">>}).
-define(PROV, lleida).

-spec send_sms(denso:phone(), denso:message(), densho:src()) -> ok | {error, densho:reason()}.

send_sms(Dst, Message, Src) ->
    send_sms(utf8, Dst, Message, Src).

-spec send_sms(densho:encoding(), denso:phone(), denso:message(), densho:src()) ->
    ok | {error, densho:reason()}.

send_sms(TypeMessage, Dst, Message, Src) ->
    {ok, ConfProviders} = application:get_env(densho, sms_providers_config),
    Conf = proplists:get_value(?PROV, ConfProviders),
    User = proplists:get_value(username, Conf),
    Pass = proplists:get_value(password, Conf),
    Url = proplists:get_value(url, Conf),
    Uri = proplists:get_value(uri, Conf),
    Body = create_body(TypeMessage, User, Pass, Dst, Message, Src),
    % TODO: implement something like poolboy to use shotgun with low latency
    %       creating several workers (not open/close every time).
    case shotgun:open(Url, 80) of
        {ok, Conn} ->
            Resp = shotgun:post(Conn, "/" ++ Uri  ++ "/" , ?HEADER,
                                list_to_binary(Body), #{}),
            shotgun:close(Conn),
            case Resp of
                {ok, Response}  ->
                    get_status(Response);
                {error, Reason} ->
                    lager:error("Error to send sms Prov: ~p Reason:~n~p",
                                [?PROV, Reason]),
                    {error, "Error to send sms Prov " ++ atom_to_list(?PROV)}
            end;
        {error, ReasonGun} ->
            lager:error("Error to send sms Prov: ~p Reason: ~p",
                        [?PROV, ReasonGun]),
            {error, ReasonGun}
    end.


%% Internal Funct

-spec get_status(Response :: map()) -> ok | {error, densho:reason()}.

get_status(Response) ->
    Content = maps:get(body, Response),
    case catch xmerl_scan:string(binary_to_list(Content)) of
        {#xmlElement{name=result}=Result, []} ->
            [Text|_] = xmerl_xpath:string("status/text()", Result),
            case lists:keyfind(Text#xmlText.value, 1, ?STATUS) of
                ?SUCCESS -> ok;
                {_, Reason} -> {error, Reason}
            end;
        Error ->
            lager:error("Error Xmerl :~p", [Error]),
            {error, "Error XML xmerl_scan"}
    end.

-type user() :: string().
-type pass() :: string().

-spec create_body(densho:encoding(), user(), pass(), densho:phone(),
                  densho:src(),densho:message()) -> string().

create_body(TypeMessage, User, Pass, Phone, Msg, Src) when is_binary(Phone) ->
    create_body(TypeMessage, User, Pass, binary_to_list(Phone), Msg, Src);

create_body(TypeMessage, User, Pass, Phone, Msg, Src) when is_binary(Msg) ->
    create_body(TypeMessage, User, Pass, Phone, binary_to_list(Msg), Src);

create_body(TypeMessage, User, Pass, Phone, Msg, Src) when is_binary(Src) ->
    create_body(TypeMessage, User, Pass, Phone, Msg, binary_to_list(Src));

create_body(utf16, User, Pass, Phone, Msg, Src) ->
    Msg64 = base64:encode(unicode:characters_to_binary(Msg, utf8, utf16)),
    "xml=" ++ edoc_lib:escape_uri("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
        "<!DOCTYPE sms SYSTEM \"sms.dtd\">"
    "<sms><user>" ++ User ++ "</user><password>" ++ Pass ++ "</password>"
        "<src>" ++ Src ++ "</src>"
        "<dst><num>" ++ Phone ++ "</num></dst>"
        "<txt encoding=\"base64\" charset=\"utf-16\">" ++ binary_to_list(Msg64) ++ "</txt>"
        "<data_coding>unicode</data_coding>"
    "</sms>");

create_body(utf8, User, Pass, Phone, Msg, Src) ->
    "xml=" ++ edoc_lib:escape_uri("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"
        "<!DOCTYPE sms SYSTEM \"sms.dtd\">"
    "<sms><user>" ++ User ++ "</user><password>" ++ Pass ++ "</password>"
        "<src>" ++ Src ++ "</src>"
        "<dst><num>" ++ Phone ++ "</num></dst>"
        "<txt>" ++ Msg ++ "</txt>"
    "</sms>").
