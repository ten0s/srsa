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
