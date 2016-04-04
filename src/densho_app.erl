-module(densho_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(shotgun),
    densho_sup:start_link().

stop(_State) ->
    ok.
