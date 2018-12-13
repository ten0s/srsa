-module(foldr_list).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec foldr(fun ((T, Acc) -> Acc), Acc, [T]) -> Acc.
%% SOLUTION_BEGIN
foldr(_Fun2, Acc, []) ->
    Acc;
foldr(Fun2, Acc, [X | Xs]) ->
    Fun2(X, foldr(Fun2, Acc, Xs)).
%% SOLUTION_END

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

append_test() ->
    L = [1,2,3,4,5],
    ?assertEqual(length(L), foldr(fun (_, Acc) -> 1 + Acc end, 0, L)),
    ?assertEqual(L, foldr(fun (X, Acc) -> [X | Acc] end, [], L)),
    ?assertEqual(lists:sum(L), foldr(fun erlang:'+'/2, 0, L)).
