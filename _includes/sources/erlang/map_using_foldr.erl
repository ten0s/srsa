-module(map_using_foldr).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

-spec map(fun ((A) -> B), [A]) -> [B].
%%+BEGIN_SOLUTION
map(Fun, Xs) ->
    lists:foldr(fun (X, Acc) -> [Fun(X) | Acc] end, [], Xs).
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

map_test() ->
    ?assertEqual([], map(fun (X) -> X + 1 end, [])),
    ?assertEqual([2,3,4,5], map(fun (X) -> X + 1 end, [1,2,3,4])).
%%+END_FOLD }
