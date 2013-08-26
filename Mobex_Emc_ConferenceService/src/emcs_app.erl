%% @author Mochi Media <dev@mochimedia.com>
%% @copyright emcs Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the emcs application.

-module(emcs_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for emcs.
start(_Type, _StartArgs) ->
    emcs_deps:ensure(),
    emcs_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for emcs.
stop(_State) ->
    ok.
