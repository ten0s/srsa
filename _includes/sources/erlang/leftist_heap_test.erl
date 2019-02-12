-module(leftist_heap_test).
-export([main/1]).

%% $ erlc leftist_heap.erl && escript leftist_heap_test.erl
%% $ ls -l leftist_heap.gif
%% -rw-r--r-- 1 ten0s ten0s 37498 Feb 12 13:22 leftist_heap.gif

main([]) ->
    L = [4,8,10,9,1,3,5,6,11],
    {H0, N} = lists:foldl(fun (T, {HAcc, I}) ->
        HAcc2 = leftist_heap:insert(T, HAcc),
        gen_png(HAcc2, I),
        {HAcc2, I+1}
    end, {leftist_heap:new(), 0}, L),
    empty_heap(fun (H, NAcc) -> gen_png(H, NAcc), NAcc+1 end, N+1, H0),
    gen_gif().

main2([]) ->
   L = [4,8,10,9,1,3,5,6,11],
   H0 = leftist_heap:from_list(L),
   empty_heap(fun (H, NAcc) -> gen_png(H, NAcc), NAcc+1 end, 0, H0),
   gen_gif().

main3([]) ->
    L = [9,8,7,6,5,4,3,2,1],
    lists:foldl(fun (T, {HAcc, I}) ->
        HAcc2 = leftist_heap:insert(T, HAcc),
        gen_png(HAcc2, I),
        {HAcc2, I+1}
    end, {leftist_heap:new(), 0}, L),
    gen_gif().

empty_heap(Fun, Acc, H) ->
    case leftist_heap:is_empty(H) of
    true ->
        Acc;
    false ->
        empty_heap(Fun, Fun(H, Acc), leftist_heap:deleteMin(H))
    end.

gen_gif() ->
    os:cmd(["convert -delay 120 -loop 0 -gravity center -background white -extent 400x450 *.png ", name(), ".gif"]),
    os:cmd("rm *.dot *.png"),
    ok.

gen_png(H, N) ->
    DotName = gen_dot(H, N),
    PngName = [name(N), ".png"],
    os:cmd(["dot -Tpng ", DotName, " -o ", PngName]).

gen_dot(H, N) ->
    File = [name(N), ".dot"],
    Dot = leftist_heap:to_dot(H),
    ok = file:write_file(File, Dot),
    File.

name(N) ->
    io_lib:format("~s-~2..0B", [name(), N]).

name() ->
    "leftist_heap".
