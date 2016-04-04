-module(densho_router).
-behaviour(gen_server).

-export([start_link/0, stop/0, get_providers/0]).

%% incluir el get_provider

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).
-include("densho.hrl").

-record(state, {sms_providers = []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:cast(?MODULE, stop).


init([]) ->
    {ok, Providers} = application:get_env(densho, sms_providers_order),
    ListProvider = lists:map(fun(Provider) ->
        Backend = get_backend(Provider),
        {Provider, Backend}
    end, Providers),
    {ok, #state{sms_providers = ListProvider}}.


handle_call(get_providers, _From, State) ->
    {reply, State#state.sms_providers, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_providers() ->
    gen_server:call(?MODULE, get_providers).

%% Internal Functions

get_backend(Provider) when is_atom(Provider) ->
    get_backend(atom_to_list(Provider));

get_backend(Provider) when is_list(Provider) ->
    list_to_atom("densho_prov_" ++ Provider).