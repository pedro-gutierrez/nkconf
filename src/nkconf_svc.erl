-module(nkconf_svc).
-export([start_services/1, load_objs/0, start/0, stop/0]).
-include("nkconf.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

start_services(_) ->
    Spec = make_service_spec(),
    case nkservice:start(?SRV, Spec) of
        {ok, _} ->
            ok;
        {error, already_started} ->
            ok;
        {error, Error} ->
            lager:error("Could not start service: ~p (~p)", [Error, Spec]),
            error(service_start_error)
    end.

start() ->
    Spec = make_service_spec(),
    nkservice:start(?SRV, Spec).

stop() ->
    nkservice:stop(?SRV).

load_objs() ->
    nkdomain_node:make_objs(objs()).

objs() -> [
           #{ path => nklib_util:bjoin(["/services", ?SRV], <<"/">>),
              name => "Conferencing",
              srv_id => ?SRV,
              ?DOMAIN_SERVICE => #{
                 spec => make_service_spec()
                }
            },
           
           #{ path => "/nkconf",
              name => "Conferencing",
              srv_id => ?SRV
            }
          ] ++ make_users( test_usernames() ).

test_usernames() -> 
    %%[ "pgr", "frr", "cgf", "amf", "amb", "jcg", "sce", "jsj", "vag", "fsj" ].
    [ "user1", "user2", "user3", "user4", "user5", "user6", "user7", "user8", "user9" ].

make_users( Users ) -> 
    [ #{ path => nklib_util:to_binary("/users/" ++ U),
         obj_id => nklib_util:to_binary(U),
         '_id' => nklib_util:to_binary(U), 
         <<"user">> => #{
             name => U,
             surname => U,
             password => "1234",
             email => U ++ "@nkconf"
            }} || U <- Users ] .

make_service_spec() ->
    Host = nkconf_app:get(listen_ip),
    Port = nkconf_app:get(listen_port),
    Path = nkconf_app:get(listen_path),
    Secure = nkconf_app:get(listen_secure),
    BinPort = nklib_util:to_binary(Port),
    Http1 = case Secure of true -> <<"https">>; false -> <<"http">> end,
    Http2 = <<Http1/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    Ws1 = case Secure of true -> <<"wss">>; false -> <<"ws">> end,
    Ws2 = <<Ws1/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,

    #{
        callback => nkconf,
        plugins => [nkapi, nkdomain],
        nkapi_server => [#{
            id => nkconf_api,
            url => <<Http2/binary, "/_api, ", Ws2/binary, "/_api/ws">>
        }],
        debug => [
        ]
    }.

