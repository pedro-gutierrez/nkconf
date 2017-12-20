-module(nkconf_api_event).
-export([event/5]).

-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

event(<<"nkconf_mediasoup">>, <<"nkconf_mediasoup">>, _, Data, Req) -> 
    send_event(<<"conferencing">>, Data, Req),
    ok;

event(ObjType, Type, _, Body, _Req) ->
    lager:warning("NKLOG NkCONF EV ~p ~p:  ~p", [ObjType, Type, Body]),
    ok.


send_event(Class, #{ status := Status} = Body, #nkreq{session_pid=Pid}) ->
    nkapi_server:send_event2(Pid, #nkevent{ class = Class,
                                      subclass = Status,
                                      body = maps:without([status], Body)}),
   ok.
