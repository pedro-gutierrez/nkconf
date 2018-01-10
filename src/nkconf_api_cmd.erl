-module(nkconf_api_cmd).
-export([cmd/2]).

-include("nkconf.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

cmd(<<"login">>, #nkreq{srv_id=?SRV, data=#{user_id:=UserId} = Data} = Req) ->
    SessData1 = maps:with([password, domain_id, meta], Data),
    SessData2 = SessData1#{
        domain_id => <<"root">>,
        id => UserId
    },
    case nkservice_api:api(<<"objects/session/start">>, SessData2, Req) of
        {ok, Reply, Req3} ->
            lager:info("Starting session: ~p", [Reply]),
            case nkconf_util:get_caller(UserId) of 
                {ok, CallerId} ->
                    {ok, Reply#{ caller_id => CallerId }, Req3};
                not_found ->
                    lager:info("User not found: ~p", [Req3]),
                    {error, no_caller_id, Req3}
            end;
        {error, object_not_found, Req3} ->
            lager:info("User not found: ~p", [Req3]),
            {error, user_not_found, Req3};
        Other -> 
            lager:info("Unexpected session start result: ~p", [Other]),
            Other
    end;

cmd(<<"action">>, #nkreq{ data=#{ reqId := ReqId, type := <<"config">> } = _Data}=Req) ->
    {ok, #{ requestTimeout => 15000,
            reqId => ReqId, 
            turnServers => [#{urls => [<<"turns:turn-cluster2.netc.io:443?transport=tcp">>],
                               credential => <<"nknk">>,
                               username => <<"turn">>}]}, Req};


cmd(<<"action">>, #nkreq{user_id=_UserId}=Req) ->
    {ok, Req2} = nkconf_mediasoup:send(Req),
    {ok, #{}, Req2};

cmd(<<"notify">>, #nkreq{user_id=_UserId}=Req) ->
    {ok, Req2} = nkconf_mediasoup:notify(Req),
    {ok, #{}, Req2};

cmd(_Cmd, Req) ->
    lager:error("NKLOG NkCONF Not Implemented ~p, ~p", [_Cmd, Req]),
    {error, not_implemented, Req}.
