-module(hdr_bin_tests_eqc).

-ifdef(TEST).
-ifdef(EQC).

-include_lib("fqc/include/fqc.hrl").

-compile(export_all).

-define(H, hdr_histogram).
-define(SF, 3).
-define(HTV, 1000000).

highest_trackable_value() ->
    choose(100000, 1000000).

significant_figures() ->
    choose(1, 3).

hist() ->
    ?LET(HTV, highest_trackable_value(), ?SIZED(Size, hist(Size, HTV))).

hist(Size, HTV) ->
    ?LAZY(oneof(
            [{call, ?MODULE, open, [HTV, significant_figures()]}
             || Size == 0]
            ++ [?LETSHRINK(
                   [H], [hist(Size-1, HTV)],
                   {call, ?MODULE, record, [H, random:uniform(HTV)]})
                || Size > 0])).

open(HVT, SF) ->
    {ok, H} = ?H:open(HVT, SF),
    H.

record(H, V) ->
    ?H:record(H, V),
    H.

values(H) ->
    [?H:min(H), ?H:mean(H), ?H:median(H), ?H:stddev(H), ?H:max(H),
     ?H:percentile(H,99.0), ?H:percentile(H, 99.9999),
     ?H:get_memory_size(H), ?H:get_total_count(H)].

prop_to_bin_none() ->
    ?FORALL(HRaw, hist(),
            begin
                H = eval(HRaw),
                Bin = ?H:to_binary(H, [{compression, none}]),
                {ok, H1} = ?H:from_binary(Bin),
                V = values(H),
                V1 = values(H1),
                ?H:close(H),
                ?H:close(H1),
                ?WHENFAIL(io:format("~p~n", [[HRaw, V, V1]]),
                          V == V1)
            end).

prop_to_bin_zlib() ->
    ?FORALL(HRaw, hist(),
            begin
                H = eval(HRaw),
                Bin = ?H:to_binary(H, [{compression, zlib}]),
                {ok, H1} = ?H:from_binary(Bin),
                V = values(H),
                V1 = values(H1),
                ?H:close(H),
                ?H:close(H1),
                ?WHENFAIL(io:format("~p~n", [[HRaw, V, V1]]),
                          V == V1)
            end).

-endif.
-endif.
