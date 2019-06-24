-module(primes).
-export([
    is_prime/1
]).

-spec is_prime(integer()) -> boolean().
is_prime(N) when N < 2 ->
    false;
is_prime(N) ->
    smallest_divisor(N) == N.

-spec smallest_divisor(pos_integer()) -> pos_integer().
smallest_divisor(N) ->
    find_divisor(N, 2).

find_divisor(N, D) when D*D > N ->
    N;
find_divisor(N, D) when N rem D == 0 ->
    D;
find_divisor(N, D) ->
    find_divisor(N, D+1).
