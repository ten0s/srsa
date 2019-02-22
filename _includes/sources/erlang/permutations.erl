-module(permutations).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

-spec perms([T]) -> [[T]].
%%+BEGIN_SOLUTION
perms([]) ->
    [[]];
perms(L) ->
    [[H | T] || H <- L, T <- perms(L -- [H])].
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

perms_test() ->
    ?assertEqual([[]], perms([])),
    ?assertEqual([[b,u,g],[b,g,u],[u,b,g],[u,g,b],[g,b,u],[g,u,b]],
                 perms([b,u,g])).
%%+END_FOLD }
