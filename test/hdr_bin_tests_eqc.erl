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
    choose(1, 5).

hist() ->
    ?LET(HTV, highest_trackable_value(),
         ?SIZED(Size, hist(Size, HTV))).

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

vals(H) ->
    maps:from_list(
      [{min, hdr_histogram:min(H)},
       {mean, hdr_histogram:mean(H)},
       {median, hdr_histogram:median(H)},
       {stddev, hdr_histogram:stddev(H)},
       {max, hdr_histogram:max(H)},
       {p99, hdr_histogram:percentile(H,99.0)},
       {p99999, hdr_histogram:percentile(H, 99.9999)},
       {mem_size, hdr_histogram:get_memory_size(H)},
       {total_count, hdr_histogram:get_total_count(H)}]).

seed() ->
    {largeint(), largeint(), largeint()}.

to_bin_opts() ->
    oneof([nc, {c, [{compression, elements([none, zlib])}]}]).
               
prop_to_bin() ->
    ?FORALL({HRaw, COpts, Seed}, {hist(), to_bin_opts(), seed()},
            begin
                random:seed(Seed),
                H = eval(HRaw),
                Bin = case COpts of
                          nc -> ?H:to_binary(H);
                          {c, Opts} -> ?H:to_binary(H, Opts)
                      end,
                Res = try ?H:from_binary(Bin) of
                          {error, Reason} -> {error, Reason};
                          {ok, H1} ->
                              V1 = vals(H1),
                              ?H:close(H1),
                              V1
                catch
                    Class:Err ->
                        {exception, Class, Err}
                end,

                V = vals(H),
                ?H:close(H),
                equals(V, Res)
            end).

-endif.
-endif.
