-module(append_list).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec append([T], [T]) -> [T].
%% SOLUTION_BEGIN
append([], Ys) ->
    Ys;
append(Xs, []) ->
    Xs;
append([X | Xs], Ys) ->
    [X | append(Xs, Ys)].
%% SOLUTION_END

-spec append([[T]]) -> [T].
%% SOLUTION_BEGIN
append(Xss) ->
    [X || Xs <- Xss, X <- Xs].
%% SOLUTION_END

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

append_2_lists_test() ->
    ?assertEqual([], append([], [])),
    ?assertEqual([3,4], append([], [3,4])),
    ?assertEqual([1,2], append([1,2], [])),
    ?assertEqual([1,2,3,4], append([1,2], [3,4])).

append_list_of_lists_test() ->
    ?assertEqual([], append([])),
    ?assertEqual([], append([[]])),
    ?assertEqual([1,2,3], append([[1,2,3], []])),
    ?assertEqual([1,2,3], append([[], [1,2,3]])),
    ?assertEqual([1,2,3,4,5,6], append([[1,2,3], [4,5,6]])).
