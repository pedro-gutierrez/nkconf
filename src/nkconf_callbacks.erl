-module(nkconf_callbacks).
-export([service_api_syntax/3, service_api_allow/2, service_api_cmd/2, service_api_event/2]).
-include("nkconf.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").


service_api_syntax(<<"conferencing_api">>, Syntax, #nkreq{cmd = <<"conferencing/", Cmd/binary>>}=Req) ->
    {conferencing_api_syntax:syntax(Cmd, Syntax), Req};

service_api_syntax(<<"conferencing_api">>, _Syntax, _Req) ->
    continue.

service_api_allow(<<"conferencing_api">>, #nkreq{cmd = <<"conferencing/login", _/binary>>, user_id = <<>>}) ->
    true;

service_api_allow(<<"conferencing_api">>, #nkreq{cmd = <<"conferencing/", _/binary>>, user_id = <<>>}) ->
    false;

service_api_allow(<<"conferencing_api">>, #nkreq{cmd = <<"conferencing/", _/binary>>}) ->
    true;

service_api_allow(<<"conferencing_api">>, _Req) ->
    continue.

service_api_cmd(<<"conferencing_api">>, #nkreq{cmd = <<"conferencing/", Cmd/binary>>}=Req) ->
    Pid = spawn_link(
        fun() ->
            Reply1 = conferencing_api_cmd:cmd(Cmd, Req#nkreq{timeout_pending=false}),
            Reply2 = case Reply1 of
                ok ->
                    {ok, #{}, Req};
                {error, Error} ->
                    {error, Error, Req};
                Other ->
                    Other
            end,
            nkservice_api:reply(Reply2)
        end),
    {ack, Pid, Req};

service_api_cmd(<<"conferencing_api">>, _Req) ->
    continue.

service_api_event(_Id, #nkreq{srv_id=?SRV, data=Data}=Req) ->
    case Data of
        #{
            class := ?DOMAIN_EVENT_CLASS,
            subclass := ObjType,
            type := Type,
            obj_id := ObjId
        } ->
            Body = maps:get(body, Data, #{}),
            conferencing_api_event:event(ObjType, Type, ObjId, Body, Req);
        _ ->
            lager:warning("NKLOG NkCONF EV SKIP ~p", [Data]),
            ok
    end;

service_api_event(_Id, #nkreq{data=Data}=_Req) ->
    lager:warning("NKLOG NkCONF EV SKIP ~p", [Data]),
    continue.
