-module(nkconf_api_syntax).
-export([syntax/2]).

syntax(<<"login">>, Syntax) ->
    Syntax#{
        user_id => binary,
        password => binary,
        domain_id => binary,
        '__mandatory' => [user_id]
    };

syntax(<<"action">>, Syntax) ->
    Syntax#{
        type => binary,
        room => binary,
        params => map,
        reqId => binary,
        '__mandatory' => [type, reqId]
    };

syntax(<<"notify">>, Syntax) ->
    Syntax#{
        room => binary,
        params => map
    };

syntax(_Cmd, Syntax) ->
    Syntax.
