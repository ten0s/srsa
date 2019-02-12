-module(append_using_foldr).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec append([T], [T]) -> [T].
%%+BEGIN_SOLUTION
append(L1, L2) ->
    lists:foldr(fun (H, T) -> [H | T] end, L2, L1).
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
