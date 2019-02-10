-module(list_zipper).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-type zlist(T) :: {[T], [T]}. % {Prev, [Curr | Next]}
-type error(_E) :: no_return().

-spec new() -> zlist(_T).
%%+BEGIN_SOLUTION
new() ->
    {[], []}.
%%+END_SOLUTION

-spec is_empty(zlist(_T)) -> boolean().
%%+BEGIN_SOLUTION
is_empty({[], []}) ->
    true;
is_empty(_) ->
    false.
%%+END_SOLUTION

-spec from_list([T]) -> zlist(T).
%%+BEGIN_SOLUTION
from_list(L) ->
    {[], L}.
%%+END_SOLUTION

-spec to_list(zlist(T)) -> [T].
%%+BEGIN_SOLUTION
to_list({Prev, Next}) ->
    lists:reverse(Prev) ++ Next.
%%+END_SOLUTION

-spec prev(zlist(T)) -> zlist(T) | error(empty).
%%+BEGIN_SOLUTION
prev({[], _}) ->
    error(empty);
prev({[P | Prev], Next}) ->
    {Prev, [P | Next]}.
%%+END_SOLUTION

-spec next(zlist(T)) -> zlist(T) | error(empty).
%%+BEGIN_SOLUTION
next({_, []}) ->
    error(empty);
next({Prev, [C | Next]}) ->
    {[C | Prev], Next}.
%%+END_SOLUTION

-spec current(zlist(T)) -> T | error(empty).
%%+BEGIN_SOLUTION
current({_, []}) ->
    error(empty);
current({_, [C | _]}) ->
    C.
%%+END_SOLUTION

-spec update(T, zlist(T)) -> zlist(T) | error(empty).
%%+BEGIN_SOLUTION
update(_V, {_, []}) ->
    error(empty);
update(V, {Prev, [_ | Next]}) ->
    {Prev, [V | Next]}.
%%+END_SOLUTION

-spec insert(T, zlist(T)) -> zlist(T).
%%+BEGIN_SOLUTION
insert(V, {Prev, Next}) ->
    {Prev, [V | Next]}.
%%+END_SOLUTION

-spec delete(zlist(T)) -> zlist(T) | error(empty).
%%+BEGIN_SOLUTION
delete({_, []}) ->
    error(empty);
delete({Prev, [_ | Next]}) ->
    {Prev, Next}.
%%+END_SOLUTION

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

zlist_test() ->
    ?assert(is_empty(new())),
    L = [4,8,10,9,1],
    ?assertEqual([], to_list(new())),
    ?assertEqual(L, to_list(from_list(L))),
    Z0 = from_list(L),
    ?assertNot(is_empty(Z0)),
    ?assertEqual(4, current(Z0)),
    ?assertError(empty, prev(Z0)),
    Z1 = next(Z0),
    ?assertEqual(8, current(Z1)),
    Z2 = update(3, Z1),
    ?assertEqual(3, current(Z2)),
    Z3 = prev(Z2),
    ?assertEqual(4, current(Z3)),
    Z4 = next(next(next(Z3))),
    Z5 = insert(2, Z4),
    ?assertEqual(2, current(Z5)),
    Z6 = prev(Z5),
    ?assertEqual(10, current(Z6)),
    Z7 = delete(Z6),
    ?assertEqual(2, current(Z7)),
    Z8 = next(next(next(Z7))),
    ?assertError(empty, current(Z8)),
    ?assertError(empty, next(Z8)),
    ?assertError(empty, delete(Z8)).
