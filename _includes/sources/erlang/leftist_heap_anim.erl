-module(leftist_heap_anim).
-export([main/1]).

main([]) ->
    L = [4,8,10,9,1,3,5,6,11],
    {H0, N} = lists:foldl(fun (T, {HAcc, I}) ->
        HAcc2 = leftist_heap:insert(T, HAcc),
        gen_dot(HAcc2, I),
        gen_cmd(io_lib:format("Insert ~p", [T]), I),
        {HAcc2, I+1}
    end, {leftist_heap:new(), 0}, L),
    empty_heap(fun (H, NAcc) ->
        gen_dot(H, NAcc),
        gen_cmd("Remove", NAcc),
        NAcc+1
    end, N+1, H0).

main2([]) ->
   L = [4,8,10,9,1,3,5,6,11],
   H0 = leftist_heap:from_list(L),
   empty_heap(fun (H, NAcc) ->
        gen_dot(H, NAcc),
        gen_cmd("Remove", NAcc),
        NAcc+1
    end, 0, H0).

main3([]) ->
    L = [9,8,7,6,5,4,3,2,1],
    lists:foldl(fun (T, {HAcc, I}) ->
        HAcc2 = leftist_heap:insert(T, HAcc),
        gen_dot(HAcc2, I),
        gen_cmd(io_lib:format("Insert ~p", [T]), I),
        {HAcc2, I+1}
    end, {leftist_heap:new(), 0}, L).

empty_heap(Fun, Acc, H) ->
    case leftist_heap:is_empty(H) of
    true ->
        Acc;
    false ->
        empty_heap(Fun, Fun(H, Acc), leftist_heap:deleteMin(H))
    end.

gen_dot(H, N) ->
    File = [name(N), ".dot"],
    Dot = leftist_heap:to_dot(H),
    ok = file:write_file(File, Dot),
    File.

gen_cmd(Cmd, N) ->
    File = [name(N), ".cmd"],
    ok = file:write_file(File, Cmd),
    File.

name(N) ->
    io_lib:format("~s-~2..0B", [name(), N]).

name() ->
    "leftist_heap".
