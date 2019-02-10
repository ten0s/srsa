-module(my_queue).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

%% Invariant must be maintained that F is empty only when R is empty.
-type queue(T) :: {F::[T], R::[T]}.
-type error(_E) :: no_return().

-spec new() -> queue(_T).
%%+BEGIN_SOLUTION
new() ->
    {[], []}.
%%+END_SOLUTION

%% O(1)
-spec is_empty(queue(_T)) -> boolean().
%%+BEGIN_SOLUTION
is_empty({[], _}) ->
    true;
is_empty({_, _}) ->
    false.
%%+END_SOLUTION

%% O(1)
-spec enqueue(T, queue(T)) -> queue(T).
%%+BEGIN_SOLUTION
enqueue(X, {[], _}) ->
    {[X], []};
enqueue(X, {F, R}) ->
    {F, [X | R]}.
%%+END_SOLUTION

-spec dequeue(queue(T)) -> {T, queue(T)} | error(empty).
%%+BEGIN_SOLUTION
dequeue({[], _}) ->
    error(empty);
dequeue({[X], R}) ->
    {X, {lists:reverse(R), []}};
dequeue({[X | F], R}) ->
    {X, {F, R}}.
%%+END_SOLUTION

%% O(1)
-spec peek(queue(T)) -> T | error(empty).
%%+BEGIN_SOLUTION
peek({[], _}) ->
    error(empty);
peek({[X | _], _}) ->
    X.
%%+END_SOLUTION

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

queue_test() ->
    Q0 = new(),
    ?assert(is_empty(Q0)),
    Q1 = {[1], []} = enqueue(1, Q0),
    ?assertNot(is_empty(Q1)),
    Q2 = {[1], [2]} = enqueue(2, Q1),
    Q3 = {[1], [3,2]} = enqueue(3, Q2),
    ?assertEqual(1, peek(Q3)),
    {1, Q4 = {[2,3], []}} = dequeue(Q3),
    Q5 = {[2,3], [4]} = enqueue(4, Q4),
    {2, Q6} = dequeue(Q5),
    {3, Q7 = {[4], []}} = dequeue(Q6),
    {4, Q8} = dequeue(Q7),
    ?assert(is_empty(Q8)),
    ?assertError(empty, peek(Q8)),
    ?assertError(empty, dequeue(Q8)).
