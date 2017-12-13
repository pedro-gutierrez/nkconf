-module(nkconf_api_event).
-export([event/5]).

-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

event(_ObjType, _Type, _SessId, _Body, _Req) ->
    lager:warning("NKLOG NkCONF EV ~s ~s ~p", [_ObjType, _Type, _Body]),
    ok.


%%send_event(Type, Body, Req) ->
%%    Event2 = #nkevent{
%%        class = <<"nkconf">>,
%%        subclass = Type,
%%        type = Type,
%%        body = Body
%%    },
%%    #nkreq{session_pid=Pid} = Req,
%%    nkapi_server:send_event2(Pid, Event2),
%%    ok.
