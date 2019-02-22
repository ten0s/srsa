-module(append_using_foldr).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

-spec append([T], [T]) -> [T].
%%+BEGIN_SOLUTION
append(Xs, Ys) ->
    lists:foldr(fun (X, Acc) -> [X | Acc] end, Ys, Xs).
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

append_test() ->
    ?assertEqual([], append([], [])),
    ?assertEqual([3,4], append([], [3,4])),
    ?assertEqual([1,2], append([1,2], [])),
    ?assertEqual([1,2,3,4], append([1,2], [3,4])).
%%+END_FOLD }
