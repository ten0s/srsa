-module(permutations).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec perms([T]) -> [[T]].
%% SOLUTION_BEGIN
perms([]) ->
    [[]];
perms(L) ->
    [[H | T] || H <- L, T <- perms(L -- [H])].
%% SOLUTION_END

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

perms_test() ->
    ?assertEqual([[]], perms([])),
    ?assertEqual([[b,u,g],[b,g,u],[u,b,g],[u,g,b],[g,b,u],[g,u,b]],
                 perms([b,u,g])).
