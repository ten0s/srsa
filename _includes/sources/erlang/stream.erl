-module(stream).
-export([
    new/0, from_list/1, to_list/1, seq/2, seq/3,
    map/2, filter/2, nth/2, take/2, drop/2, zip/2,
    cycle/1, iterate/2
]).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

-type promise(T) :: fun (() -> T).
-type stream(T) :: {} | {T, promise(T)}.
-type error(_E) :: no_return().

-define(DELAY(E), fun () -> E end).
-define(FORCE(P), P()).

-spec new() -> stream(_T).
%%+BEGIN_SOLUTION
new() ->
    {}.
%%+END_SOLUTION

-spec from_list([T]) -> stream(T).
%%+BEGIN_SOLUTION
from_list([]) ->
    {};
from_list([X | Xs]) ->
    {X, ?DELAY(from_list(Xs))}.
%%+END_SOLUTION

-spec to_list(stream(T)) -> [T].
%%+BEGIN_SOLUTION
to_list({}) ->
    [];
to_list({X, P}) ->
    [X | to_list(?FORCE(P))].
%%+END_SOLUTION

-spec seq(integer(), integer()) -> stream(integer()).
seq(From, To) ->
    seq(From, To, 1).

-spec seq(integer(), integer(), integer()) -> stream(integer()).
seq(From, To, Step) when (From < To andalso Step > 0) orelse
                         (From > To andalso Step < 0) ->
    {From, ?DELAY(seq(From + Step, To, Step))};
seq(From, From, _) ->
    {From, ?DELAY({})};
seq(_, _, _) ->
    error(badarg).

-spec map(fun ((T) -> U), stream(T)) -> stream(U).
%%+BEGIN_SOLUTION
map(_Fun, {}) ->
    {};
map(Fun, {X, P}) ->
    {Fun(X), ?DELAY(map(Fun, ?FORCE(P)))}.
%%+END_SOLUTION

-spec filter(fun ((T) -> boolean()), stream(T)) -> stream(T).
%%+BEGIN_SOLUTION
filter(_Pred, {}) ->
    {};
filter(Pred, {X, P}) ->
    case Pred(X) of
    true ->
        {X, ?DELAY(filter(Pred, ?FORCE(P)))};
    false ->
        filter(Pred, ?FORCE(P))
    end.
%%+END_SOLUTION

-spec nth(pos_integer(), stream(T)) -> T | error(empty).
%%+BEGIN_SOLUTION
nth(_, {}) ->
    error(empty);
nth(1, {X, _}) ->
    X;
nth(N, {_, P}) when N > 1 ->
    nth(N-1, ?FORCE(P)).
%%+END_SOLUTION

-spec take(pos_integer(), stream(T)) -> stream(T).
%%+BEGIN_SOLUTION
take(_, {}) ->
    {};
take(0, _) ->
    {};
take(N, {X, P}) when N > 0 ->
    {X, ?DELAY(take(N-1, ?FORCE(P)))}.
%%+END_SOLUTION

-spec drop(pos_integer(), stream(T)) -> stream(T).
%%+BEGIN_SOLUTION
drop(_, {}) ->
    {};
drop(0, {X, P}) ->
    {X, P};
drop(N, {_, P}) when N > 0 ->
    drop(N-1, ?FORCE(P)).
%%+END_SOLUTION

-spec zip(stream(T), stream(U)) -> stream({T, U}).
%%+BEGIN_SOLUTION
zip(_, {}) ->
    {};
zip({}, _) ->
    {};
zip({X, P}, {Y, R}) ->
    {{X, Y}, ?DELAY(zip(?FORCE(P), ?FORCE(R)))}.
%%+END_SOLUTION

-spec cycle([T]) -> stream(T).
%%+BEGIN_SOLUTION
cycle([]) ->
    {};
cycle(L) ->
    cycle(L, L).

cycle([], L) ->
    cycle(L, L);
cycle([X | Xs], L) ->
    {X, ?DELAY(cycle(Xs, L))}.
%%+END_SOLUTION

-spec iterate(fun ((T) -> T), T) -> stream(T).
%%+BEGIN_SOLUTION
iterate(Next, Init) ->
    {Init, ?DELAY(iterate(Next, Next(Init)))}.
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

from_to_test() ->
    ?assertEqual([], to_list(new())),
    ?assertEqual([1,2,3,4], to_list(from_list([1,2,3,4]))).

seq_test() ->
    ?assertEqual([1,2,3], to_list(seq(1,3))),
    ?assertEqual([1,0,-1], to_list(seq(1,-1,-1))).

s1_to_4() ->
    seq(1, 4).

s1_to_100() ->
    seq(1, 100).

map_test() ->
    ?assertEqual([], to_list(map(fun (X) -> X*X end, new()))),
    ?assertEqual([1,4,9,16], to_list(map(fun (X) -> X*X end, s1_to_4()))).

filter_test() ->
    ?assertEqual([], to_list(filter(fun (X) -> X rem 2 == 0 end, new()))),
    ?assertEqual([2,4], to_list(filter(fun (X) -> X rem 2 == 0 end, s1_to_4()))).

filter_map_test() ->
    ?assertEqual([4,16], to_list(
        filter(fun (X) -> X rem 2 == 0 end,
            map(fun (X) -> X*X end,
                s1_to_4())))).

nth_test() ->
    ?assertError(empty, nth(1, new())),
    ?assertEqual(1, nth(1, s1_to_4())),
    ?assertEqual(4, nth(4, s1_to_4())),
    ?assertError(empty, nth(5, s1_to_4())).

take_test() ->
    ?assertEqual([], to_list(take(100, new()))),
    ?assertEqual([], to_list(take(0, s1_to_100()))),
    ?assertEqual([1,2,3], to_list(take(3, s1_to_100()))).

drop_test() ->
    ?assertEqual([], to_list(drop(100, new()))),
    ?assertEqual([98,99,100], to_list(drop(97, s1_to_100()))),
    ?assertEqual([], to_list(drop(100, s1_to_100()))).

zip_test() ->
    ?assertEqual([], to_list(zip(s1_to_4(), new()))),
    ?assertEqual([], to_list(zip(new(), s1_to_4()))),
    ?assertEqual([{1,2},{2,4},{3,6},{4,8}],
        to_list(zip(s1_to_4(), map(fun (X) -> 2*X end, s1_to_4())))).

cycle_test() ->
    ?assertEqual([], to_list(cycle([]))),
    ?assertEqual([3,1,2], to_list(take(3, drop(2, cycle([1,2,3]))))).

iterate_test() ->
    Ints = iterate(fun (X) -> X+1 end, 0),
    ?assertEqual([0,1,2], to_list(take(3, Ints))),
    ?assertEqual([10,11,12], to_list(take(3, drop(10, Ints)))).
%%+END_FOLD }
