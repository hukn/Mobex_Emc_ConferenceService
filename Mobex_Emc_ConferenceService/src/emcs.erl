%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc emcs.

-module(emcs).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the emcs server.
start() ->
    emcs_deps:ensure(),
    ensure_started(crypto),
    application:start(emcs).


%% @spec stop() -> ok
%% @doc Stop the emcs server.
stop() ->
    application:stop(emcs).
