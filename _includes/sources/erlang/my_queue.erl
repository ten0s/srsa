-module(my_queue).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

%% Invariant must be maintained that F is empty only when R is empty.
-type queue(T) :: {F::[T], R::[T]}.
-type error(_E) :: no_return().

-spec new() -> queue(_T).
%% SOLUTION_BEGIN
new() ->
    {[], []}.
%% SOLUTION_END

%% O(1)
-spec is_empty(queue(_T)) -> boolean().
%% SOLUTION_BEGIN
is_empty({[], _}) ->
    true;
is_empty(_) ->
    false.
%% SOLUTION_END

%% O(1)
-spec enqueue(T, queue(T)) -> queue(T).
%% SOLUTION_BEGIN
enqueue(X, {[], _}) ->
    {[X], []};
enqueue(X, {F, R}) ->
    {F, [X | R]}.
%% SOLUTION_END

-spec dequeue(queue(T)) -> {T, queue(T)} | error(empty).
%% SOLUTION_BEGIN
dequeue({[], _}) ->
    error(empty);
dequeue({[X], R}) ->
    {X, {lists:reverse(R), []}};
dequeue({[X | F], R}) ->
    {X, {F, R}}.
%% SOLUTION_END

%% O(1)
-spec peek(queue(T)) -> T | error(empty).
%% SOLUTION_BEGIN
peek({[], _}) ->
    error(empty);
peek({[X | _], _}) ->
    X.
%% SOLUTION_END

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

queue_test() ->
    Q0 = new(),
    true = is_empty(Q0),
    Q1 = {[1], []} = enqueue(1, Q0),
    false = is_empty(Q1),
    Q2 = {[1], [2]} = enqueue(2, Q1),
    Q3 = {[1], [3,2]} = enqueue(3, Q2),
    1 = peek(Q3),
    {1, Q4 = {[2,3], []}} = dequeue(Q3),
    Q5 = {[2,3], [4]} = enqueue(4, Q4),
    {2, Q6} = dequeue(Q5),
    {3, Q7 = {[4], []}} = dequeue(Q6),
    {4, Q8} = dequeue(Q7),
    true = is_empty(Q8),
    {'EXIT', {empty, _}} = (catch peek(Q8)),
    {'EXIT', {empty, _}} = (catch dequeue(Q8)).
