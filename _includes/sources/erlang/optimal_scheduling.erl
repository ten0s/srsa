-module(optimal_scheduling).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

-type from() :: integer().
-type to()   :: integer().
-type interval() :: {from(), to()}.

-spec schedule([interval()]) -> [interval()].
%%+BEGIN_SOLUTION
schedule(Intervals) ->
    Intervals2 = lists:sort(
        fun ({_, To1}, {_, To2}) -> To1 =< To2 end, Intervals),
    schedule(Intervals2, []).

schedule([], Acc) ->
    lists:reverse(Acc);
schedule([Interval | Intervals], Acc) ->
    Intervals2 = lists:dropwhile(fun (I) -> overlap(I, Interval) end, Intervals),
    schedule(Intervals2, [Interval | Acc]).

overlap({X1,X2}, {Y1,Y2}) ->
    max(X1,Y1) =< min(X2,Y2).
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

schedule_test() ->
    ?assertEqual([], schedule([])),
    ?assertEqual([
            {0,1}
        ], schedule([
            {0,1}
        ])),
    ?assertEqual([
            {1,3},
            {4,8},
            {11,13},
            {14,17}
        ], schedule([
            {0,5},
            {1,3},
            {2,7},
            {4,8},
            {6,10},
            {9,14},
            {11,13},
            {12,16},
            {14,17}
       ])),
    ?assertEqual([
            {1,2},
            {3,4},
            {5,6},
            {7,8},
            {9,10}
        ], schedule([
            {0,11},
            {1,2},
            {3,4},
            {5,6},
            {7,8},
            {9,10}
        ])),
    ?assertEqual([
            {0,2},
            {3,5}
        ], schedule([
            {0,2},
            {1,4},
            {3,5}
        ])).
%%+END_FOLD }
