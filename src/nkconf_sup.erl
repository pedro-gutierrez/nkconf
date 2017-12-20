-module(nkconf_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

init([Config]) ->
    {ok, { {one_for_all, 0, 1}, [ child_spec(nkconf_mediasoup, [Config])]} }.

child_spec(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}.
