-module(densho_prov_lleida).
-behaviour(densho_prov).

-export([send_sms/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include("lleida.hrl").

-define(HEADER, #{<<"content-type">> => <<"application/x-www-form-urlencoded">>}).
-define(PROV, lleida).

send_sms(Dst, Message) ->
    {ok, ConfProviders} = application:get_env(densho, sms_providers_config),
    Conf = proplists:get_value(?PROV, ConfProviders),
    User = proplists:get_value(username, Conf),
    Pass = proplists:get_value(password, Conf),
    Url = proplists:get_value(url, Conf),
    Uri = proplists:get_value(uri, Conf),
    Body = create_body(User, Pass, Dst, Message),
    {ok, Conn} = shotgun:open(Url, 80),
    {ok, Response} = shotgun:post(Conn, "/" ++ Uri  ++ "/" , ?HEADER, list_to_binary(Body), #{}),
    shotgun:close(Conn),
    get_status(Response).




%% Internal Funct

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
            {error, "Error XML xmer_scan"}
    end.

create_body(User, Pass, Phone, Msg) when is_binary(Phone) ->
    create_body(User, Pass, binary_to_list(Phone), Msg);

create_body(User, Pass, Phone, Msg) when is_binary(Msg) ->
    create_body(User, Pass, Phone, binary_to_list(Msg));

create_body(User, Pass, Phone, Msg) ->
    "xml=" ++ edoc_lib:escape_uri("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><!DOCTYPE sms SYSTEM \"sms.dtd\">"
    "<sms><user>" ++ User ++ "</user><password>" ++ Pass ++ "</password><dst><num>+34" ++ Phone ++
    "</num></dst><txt>" ++ Msg ++ "</txt></sms>").
