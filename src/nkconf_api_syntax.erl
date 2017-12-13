-module(nkconf_api_syntax).
-export([syntax/2]).

syntax(<<"login">>, Syntax) ->
    Syntax#{
        user_id => binary,
        password => binary,
        domain_id => binary,
        '__mandatory' => [user_id]
    };

syntax(_Cmd, Syntax) ->
    Syntax.
