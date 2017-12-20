-module(nkconf_mediasoup).
-behaviour(gen_statem).
-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([send/1, notify/1, status/0]).
-export([response/1]).
-export([connecting/3, connected/3]).
-record(data, {config, callback, ref, pid}).
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").

callback_mode() ->
    state_functions.

start_link(Config) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Config], []).

init([#{ mediasoup := Config}]) ->
    {ok, connecting, #data{config=Config,
                           ref=undefined,
                           pid=undefined,
                           callback={ ?MODULE, response, []}}, [{{timeout, init},0,connect}]}.

connect(#data{ref=Ref}=Data) when Ref =/= undefined ->
    debug(connecting, {demonitor, Ref}),
    erlang:demonitor(Ref),
    connect(Data#data{ref=undefined});

connect(#data{config=Config, callback=CB}=Data) ->
    case nkconf_mediasoup_protocol:start(#{ config => Config}, CB) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            info(connecting, {connected, Pid, Ref}),
            {next_state, connected, Data#data{ref=Ref, pid=Pid}};
        {error, Error} ->
            warn(connecting, {error, Error}),
            {next_state, connecting, Data, 
                [{{timeout, init},1000,connect}]}
    end.

connecting({timeout, init}, connect, Data) ->
    connect(Data);

connecting(info, {'DOWN', Ref, process, Pid, Reason}, #data{ref=Ref, pid=Pid}=Data) -> 
    warn(connecting, {down, same, Pid, Ref, Reason}),
    connect(Data);

connecting(info, {'DOWN', Ref, process, Pid, Reason}, Data) -> 
    warn(connecting, {down, other, Pid, Ref, Reason}),
    {keep_state, Data};

connecting({call, From}, status, Data) -> 
    {keep_state, Data, [{reply, From, {ok, #{ status => connecting }}}]}.

connected(info, {'DOWN', Ref, process, Pid, Reason}, #data{ref=Ref, pid=Pid}=Data) -> 
    warn(connected, {down, same, Pid, Ref, Reason}),
    connect(Data);

connected(info, {'DOWN', Ref, process, Pid, Reason}, Data) -> 
    info(connected, {down, other, Pid, Ref, Reason}),
    {keep_state, Data};

connected(cast, {response, [connected, Pid, _]}, #data{pid=Pid}=Data) ->
    {keep_state, Data};

connected(cast, {response, [connected, Pid2, _]}, #data{pid=Pid}=Data) ->
    warn(connected, {reconnection, Pid, Pid2}),
    {keep_state, Data};

connected(cast, {response, [disconnected, Pid, _]}, #data{pid=Pid}=Data) ->
    connect(Data);

connected(cast, {response, [disconnected, Pid, _]}, Data) ->
    info(connected, {disconnected, other, Pid}),
    {keep_state, Data};

connected(cast, {response, [ok, Pid, #{ <<"status">> := Status,
                                        <<"roomID">> := Room,
                                        <<"peerID">> := UserId,
                                        <<"method">> := Method } = Msg]}, #data{pid=Pid}=Data) ->
    EvBody = #{ status => Status,
                action => Method,
                room => Room },

    EvBody2 = case maps:is_key(<<"data">>, Msg) of
                 true -> 
                     EvBody#{ data => maps:get(<<"data">>, Msg) };
                 false -> 
                     EvBody
            end,
    EvBody3 = case maps:is_key(<<"reqID">>, Msg) of
                  true -> EvBody2#{ reqId => maps:get(<<"reqID">>, Msg)};
                  false -> EvBody2
              end,

    debug(connected, {Status, Method, Room}), 
    Ev = event(UserId, EvBody3),
    nkevent:send(Ev),
    {next_state, connected, Data};

connected(cast, {response, [ok, Pid, Msg]}, #data{pid=Pid}=Data) -> 
    debug(connected, {unknown_response, Pid, Msg}),
    {keep_state, Data};

connected({call, From}, status, Data) -> 
    {keep_state, Data, [{reply, From, {ok, #{ status => connected }}}]};


connected({call, From}, {send, #{ type := <<"request">>,
                                  reqId := RequestId,
                                  room := Room,
                                  user_id := UserId,
                                  params := Params }}, #data{pid=Pid}=Data) ->
    
    Req2 = #{ cmd => 'mediasoup-request',
              roomID => Room,
              peerID => UserId,
              reqID => RequestId,
              internal => Params },
    nkconf_mediasoup_protocol:send(Pid, Req2),
    {keep_state, Data, [{reply, From, ok}]};

connected({call, From}, {send, #{ type := Cmd, 
                                  reqId := ReqId,
                                  user_id := UserId } = Req},  #data{pid=Pid}=Data) ->
    Req2 = #{ cmd => Cmd,
              reqID => ReqId,
              peerID => UserId,
              tid => 0 },

    Req3 = case maps:is_key(room, Req) of 
               false -> Req2;
               true -> 
                   Req2#{ roomID => maps:get(room, Req)}
           end,
    nkconf_mediasoup_protocol:send(Pid, Req3),
    {keep_state, Data, [{reply, From, ok}]};

connected({call, From}, {notify, #{ room := Room,
                                    user_id := UserId,
                                    params := Params }}, #data{pid=Pid}=Data) ->
    
    Req2 = #{ cmd => 'mediasoup-notification',
              roomID => Room,
              peerID => UserId,
              tid => 0,
              internal => Params },
    nkconf_mediasoup_protocol:send(Pid, Req2),
    {keep_state, Data, [{reply, From, ok}]}.



terminate(Reason, _, Data) ->
    warn(terminate, {Reason, Data}),
    ok.

status() -> 
    gen_statem:call(?MODULE, status).

response(Msg) ->
    debug(response, Msg),
    gen_statem:cast(?MODULE, {response, Msg}).

send(#nkreq{user_id=UserId, data=Data}=Req) ->
    Ev = event(Req),
    {ok, _, Req2} = nkservice_api:api(<<"event/subscribe">>, Ev, Req),
    ok = gen_statem:call(?MODULE, {send, Data#{user_id => UserId}}),
    {ok, Req2}.

notify(#nkreq{user_id=UserId, data=Data}=Req) ->
    Ev = event(Req),
    {ok, _, Req2} = nkservice_api:api(<<"event/subscribe">>, Ev, Req),
    ok = gen_statem:call(?MODULE, {notify, Data#{user_id => UserId}}),
    {ok, Req2}.

event(#nkreq{user_id=UserId}) -> 
    event(UserId);

event(UserId) -> 
    #{ srv_id => nkconf,
       class => ?DOMAIN_EVENT_CLASS,
       subclass => <<"nkconf_mediasoup">>,
       type => <<"nkconf_mediasoup">>,
       obj_id => UserId }.

event(UserId, Data) ->
    #nkevent{class=?DOMAIN_EVENT_CLASS,
             subclass= <<"nkconf_mediasoup">>,
             type= <<"nkconf_mediasoup">>,
             obj_id= UserId,
             body = Data,
             srv_id= nkconf}.

log(_Level, State, Term) ->
    lager:info("NkCONF mediasoup [~p]: ~p", [State, Term]).

info(State, Term) ->
    log(info, State, Term).

warn(State, Term) ->
    log(warning, State, Term).

debug(State, Term) ->
    log(debug, State, Term).
