-module(nkconf_util).
-export([get_caller/1]).

get_caller(UserId) ->
    Callers = nkconf_app:get(callers, []),
    case lists:keyfind(UserId, 1, Callers) of
        {UserId, CallerId} -> 
            {ok, CallerId};
        false ->
            not_found
    end.
