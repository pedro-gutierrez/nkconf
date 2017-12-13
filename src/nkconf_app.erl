-module(nkconf_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([get/1, get/2]).
-define(APP, nkconf).

start(_StartType, _StartArgs) ->
    Syntax = #{
        listen_ip => host,
        listen_port => {integer, 1, 65535},
        listen_path => basepath,
        listen_secure => boolean,
        '__defaults' => #{
          listen_ip => <<"127.0.0.1">>,
          listen_port => 9301,
          listen_path => <<"/">>,
          listen_secure => false
        }
    },
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = nkconf_sup:start_link(),
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("NkCONF v~s has started.", [Vsn]),
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error(Error)
    end.

stop(_State) ->
    ok.

get(Key) ->
    nklib_config:get(?APP, Key).

get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).
