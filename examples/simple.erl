#!/usr/bin/env escript
%%! -sname hdr_histogram_simple -pa ebin

-mode(compile).

%
% Simple histogram capture example using Erlang
%

loop(_,0) -> ok;
loop(R,X) -> hdr_histogram:record(R,random:uniform(1000000)), loop(R,X-1).

main(_) ->
    {ok,R} = hdr_histogram:open(1000000,3),
    N = 10000000, %2.2 million/sec on my laptop but YMMV

    %% record a random uniform distribution of 1M data points
    S = os:timestamp(),
    loop(R, N),
    E = os:timestamp(),
    X = timer:now_diff(E,S)/1.0e6,
    Y = case X>1 of true -> N/X; false -> N*X end,
    io:format("Runtime: ~psecs ~.5frps~n", [X,Y]),

    %% print percentiles to stdout as CSV
    hdr_histogram:print(R,csv),

    %% print percentiles to stdout as CLASSIC
    hdr_histogram:log(R,classic,"erlang.hgrm"),

    io:format("Min ~p~n", [hdr_histogram:min(R)]),
    io:format("Mean ~.3f~n", [hdr_histogram:mean(R)]),
    io:format("Median ~.3f~n", [hdr_histogram:median(R)]),
    io:format("Max ~p~n", [hdr_histogram:max(R)]),
    io:format("Stddev ~.3f~n", [hdr_histogram:stddev(R)]),
    io:format("99ile ~.3f~n", [hdr_histogram:percentile(R,99.0)]),
    io:format("99.9999ile ~.3f~n", [hdr_histogram:percentile(R,99.9999)]),
    io:format("Memory Size ~p~n", [hdr_histogram:get_memory_size(R)]),
    io:format("Total Count ~p~n", [hdr_histogram:get_total_count(R)]),

    %% we're done, cleanup any held resources
    hdr_histogram:close(R),

    io:format("Done!\n").
