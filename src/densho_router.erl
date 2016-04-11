-module(densho_router).

-export([get_providers/0]).

-spec get_providers() -> densho:routes().

get_providers() ->
    {ok, Providers} = application:get_env(densho, sms_providers_order),
    lists:map(fun(Provider) ->
        Backend = get_backend(Provider),
        {Provider, Backend}
    end, Providers).

-spec get_backend(densho:provider()) -> module().

get_backend(Provider) when is_atom(Provider) ->
    get_backend(atom_to_list(Provider));

get_backend(Provider) when is_list(Provider) ->
    list_to_atom("densho_prov_" ++ Provider).
