-module(nkconf_mediasoup_protocol).
-export([register/0]).
-export([transports/1, default_port/1, conn_init/1, conn_parse/3, conn_encode/2]).
-export([start/2, send/2]).
-include_lib("nkpacket/include/nkpacket.hrl").

transports(_) -> [ws, wss].
default_port(ws) -> 1803;
default_port(wss) -> 443.

register() ->
    nkpacket:register_protocol(mediasoup, ?MODULE).

start(#{config := #{ host := Host,
                     scheme := Scheme,
                     port := Port,
                     path := Path,
                     username := User,
                     password := Password }}, Callback) ->
    AuthToken = base64:encode_to_string(
                 binary_to_list(<<User/binary, <<":">>/binary, Password/binary>>)),
    Headers = [{"Authorization", "Basic " ++ AuthToken}],
    Url = url(Scheme, Host, Port, Path),
    start(Url, Callback, Headers);

start(#{config := #{ host := Host,
                     scheme := Scheme,
                     port := Port,
                     path := Path }}, Callback) ->
    Url = url(Scheme, Host, Port, Path),
    start(Url, Callback, []).

start(Server, Callback, Headers) ->
    ConnOpts = #{
      class => mediasoup,
      headers => Headers,
      connect_timeout => 60000,
      idle_timeout => 30000,
      protocol => ?MODULE,
      user_state => #{ callback => Callback },
      debug => false },
    lager:info("starting connection to mediasoup on ~p", [Server]),
    case nkpacket:connect(Server, ConnOpts) of
        {ok, Pid} ->
            lager:info("Connected to mediasoup with pid ~p", [Pid]),
            {ok, Pid};
        {error, Error} ->
            lager:debug("Could not connect to mediasoup: ~p", [Error]),
            {error, Error}
    end.

conn_init(#nkport{pid=Pid}=NkPort) ->
    {ok, #{callback := CB}=UserData } = nkpacket:get_user_state(NkPort),
    callback(CB, connected, Pid),
    {ok, UserData}.

conn_parse(close, #nkport{pid=Pid}, #{callback := CB}=State) ->
    callback(CB, disconnected, Pid),
    {ok, State};

conn_parse({text, Text}, #nkport{pid=Pid}, #{callback := CB}=State) ->
    case nklib_json:decode(Text) of
        {error, E} ->
            callback(CB, error, Pid, E);
        Data when is_map(Data) ->
            notify(Data, CB, Pid);
        Other ->
            callback(CB, error, Pid, {unexpected, Other})
    end,
    {ok, State}.

notify(Msg, CB, Pid) ->
    callback(CB, ok, Pid, Msg).

conn_encode(Msg, _NkPort) when is_map(Msg) ->
    case nklib_json:encode(Msg) of
        error ->
            lager:warning("invalid json in ~p: ~p", [?MODULE, Msg]),
            {error, invalid_json};
        Json ->
            {ok, {text, Json}}
    end.

send(Conn, Msg) when is_map(Msg) ->
    nkpacket:send(Conn, Msg);

send(_, Msg) ->
    {error, invalid_format, Msg}.

url(Scheme, Host, Port, Path) ->
    string:join([nklib_util:to_list(Scheme),
                 "://",
                 nklib_util:to_list(Host),
                 ":",
                 nklib_util:to_list(Port),
                 nklib_util:to_list(Path)], "").

callback(CB, Event, Pid) ->
    callback(CB, Event, Pid, none).

callback({M, F, A}, Event, Pid, Msg) ->
    M:F(A ++ [Event, Pid, Msg]).
