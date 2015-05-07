-module(hdr_histogram_SUITE).

-export([all/0]).
-export([groups/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([group/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-define(root,?config(data_dir,Config)).

-export([t_hdr_create/1]).
-export([t_hdr_invalid_sigfig/1]).
-export([t_hdr_total_count/1]).
-export([t_hdr_max_value/1]).
-export([t_hdr_min_value/1]).
-export([t_hdr_percentiles/1]).
-export([t_hdr_reset/1]).
-export([t_hdr_close/1]).
-export([t_hdr_binary/1]).
-export([t_hdr_binary_nc/1]).
-export([t_iter_recorded/1]).
-export([t_iter_linear/1]).
-export([t_iter_logarithmic/1]).
-export([t_iter_percentile/1]).
-export([t_counter_example_stddev/1]).
-export([t_issue_004/1]).
-export([t_issue_013/1]).
-export([t_unique_resource_types/1]).

-export([load_histograms/0]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, hdr}
     , {group, iter}
     , {group, counter}
     , {group, regression}
    ].

groups() ->
    [{hdr, [], [
        t_hdr_create
      , t_hdr_invalid_sigfig
      , t_hdr_total_count
      , t_hdr_max_value
      , t_hdr_min_value
      , t_hdr_percentiles
      , t_hdr_reset
      , t_hdr_close
      , t_hdr_binary
      , t_hdr_binary_nc %% Commented out. Issues arize when used with CT
    ]},
     {iter, [], [
        t_iter_recorded
      , t_iter_linear
      , t_iter_logarithmic
      , t_iter_percentile
    ]},
     %% Counter examples / regression tests for bugs
    {counter, [], [
        t_counter_example_stddev
    ]},
    {regression, [], [
        t_issue_004,
        t_issue_013,
        t_unique_resource_types
    ]}].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 2000}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    {Raw,Cor} = load_histograms(),
    [{raw,Raw},{cor,Cor}|Config].

end_per_group(_GroupName, Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    hdr_histogram:close(Raw),
    hdr_histogram:close(Cor),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

t_hdr_create(_Config) ->
    {ok,R} = hdr_histogram:open(36000000, 4),
    1704008 = hdr_histogram:get_memory_size(R),
    hdr_histogram:close(R),
    ok.

t_hdr_invalid_sigfig(_Config) ->
    {error,bad_significant_factor} = (catch hdr_histogram:open(36000000, -1)),
    {error,bad_significant_factor} = (catch hdr_histogram:open(36000000, 6)),
    ok.

t_hdr_total_count(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    10001 = hdr_histogram:get_total_count(Raw),
    20000 = hdr_histogram:get_total_count(Cor),
    ok.

t_hdr_max_value(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    RawMax = hdr_histogram:max(Raw),
    CorMax = hdr_histogram:max(Cor),
    hdr_histogram:same(Raw,100000000,RawMax),
    hdr_histogram:same(Cor,100000000,CorMax),
    ok.

t_hdr_min_value(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    1000 = hdr_histogram:min(Raw),
    1000 = hdr_histogram:min(Cor),
    ok.

t_hdr_percentiles(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    cmp(1.0e3 , hdr_histogram:percentile(Raw, 30.0), 0.001),
    cmp(1.0e3 , hdr_histogram:percentile(Raw, 99.0), 0.001),
    cmp(1.0e3 , hdr_histogram:percentile(Raw, 99.99), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Raw, 99.999), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Raw, 100.0), 0.001),
    cmp(1.0e3 , hdr_histogram:percentile(Cor, 30.0), 0.001),
    cmp(5.0e7 , hdr_histogram:percentile(Cor, 75.0), 0.001),
    cmp(8.0e7 , hdr_histogram:percentile(Cor, 90.0), 0.001),
    cmp(9.8e7 , hdr_histogram:percentile(Cor, 99.0), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Cor, 99.99), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Cor, 99.999), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Cor, 100.0), 0.001),
    ok.

t_hdr_reset(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    hdr_histogram:reset(Raw),
    hdr_histogram:reset(Cor),
    0 = hdr_histogram:get_total_count(Raw),
    0 = hdr_histogram:get_total_count(Cor),
    0.0 = hdr_histogram:percentile(Raw, 99.0),
    0.0 = hdr_histogram:percentile(Cor, 99.0),
    ok.

t_hdr_close(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    hdr_histogram:close(Raw),
    hdr_histogram:close(Cor),
    %% double close is harmless
    ok.

t_hdr_binary(_Config) ->
    %% Work around an issue with CT and NIFs
    {Raw,Cor} = load_histograms(),
    BinRaw = hdr_histogram:to_binary(Raw),
    BinCor = hdr_histogram:to_binary(Cor),
    true = is_binary(BinRaw),
    true = is_binary(BinCor),
    {ok,Raw2} = hdr_histogram:from_binary(BinRaw),
    {ok,Cor2} = hdr_histogram:from_binary(BinCor),
    {error,bad_hdr_binary} = (catch hdr_histogram:from_binary(<<>>)),
    cmp(1.0e8 , hdr_histogram:percentile(Raw, 100.0), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Raw2, 100.0), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Cor, 100.0), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Cor2, 100.0), 0.001),
    ok.

t_hdr_binary_nc(_Config) ->
    %% Work around an issue with CT and NIFs
    {Raw,Cor} = load_histograms(),
    BinRaw = hdr_histogram:to_binary(Raw, [{compression, none}]),
    BinCor = hdr_histogram:to_binary(Cor, [{compression, none}]),
    true = is_binary(BinRaw),
    true = is_binary(BinCor),
    {ok,Raw2} = hdr_histogram:from_binary(BinRaw),
    {ok,Cor2} = hdr_histogram:from_binary(BinRaw),
    {error,bad_hdr_binary} = (catch hdr_histogram:from_binary(<<>>)),
    cmp(1.0e8 , hdr_histogram:percentile(Raw, 100.0), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Raw2, 100.0), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Cor, 100.0), 0.001),
    cmp(1.0e8 , hdr_histogram:percentile(Cor2, 100.0), 0.001),
    ok.


t_iter_recorded(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(record, Raw, []), 
  RawStepCounts = hdr_iter:each(RawIter, step_counts(), []),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(record, Cor, []), 
  CorStepCounts = hdr_iter:each(CorIter, step_counts(), []),
  hdr_iter:close(CorIter),

  [10000,1] = RawStepCounts,
  [10000|X] = CorStepCounts,
  10000 = lists:sum(X),

  10001 = lists:sum(RawStepCounts),
  20000 = lists:sum(CorStepCounts),
  ok.

t_iter_linear(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(linear, Raw, [{linear_value_unit,10000}]), 
  RawStepCounts = hdr_iter:each(RawIter,step_counts(), []),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(linear, Cor, [{linear_value_unit,10000}]), 
  CorStepCounts = hdr_iter:each(CorIter,step_counts(), []),
  hdr_iter:close(CorIter),

  1 = lists:sum(RawStepCounts),
  20000 = lists:sum(CorStepCounts),
  ok.

t_iter_logarithmic(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(logarithmic, Raw, [{log_value_unit,100},{log_base,10.0}]), 
  RawStepCounts = hdr_iter:each(RawIter,accum_steps(), 0),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(logarithmic, Cor, [{log_value_unit,100},{log_base,10.0}]), 
  CorStepCounts = hdr_iter:each(CorIter,accum_steps(), 0),
  hdr_iter:close(CorIter),

  10001 = RawStepCounts,
  20000 = CorStepCounts,
  ok.

t_iter_percentile(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(percentile, Raw, [{percentile_half_ticks,20}]), 
  RawStepCounts = hdr_iter:each(RawIter,count(), 0),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(percentile, Cor, [{percentile_half_ticks,20}]), 
  CorStepCounts = hdr_iter:each(CorIter,count(), 0),
  hdr_iter:close(CorIter),

  270 = RawStepCounts,
  238 = CorStepCounts,
  ok.

%% This test triggers a bug in the stddev function.
t_counter_example_stddev(_Config) ->
    {ok, H} = hdr_histogram:open(100000,1),
    hdr_histogram:record(H, 52581),
    StdDev = hdr_histogram:stddev(H),
    %% StdDev is a ilegal value we need to print it to tigger the crash
    %% or convert it to binary and back. the binery conversion seems to
    %% be the nicer choice.
    %% io:format("Stddev is: ~p~n", [StdDev]),
    binary_to_term(term_to_binary(StdDev)),
    hdr_histogram:close(H),
    ok.

t_issue_004(_Config) ->
    {ok,R} = hdr_histogram:open(10,1),
    [ begin
        ok = hdr_histogram:record(R, X)
    end || X <- lists:seq(0,10) ],
    {error, value_out_of_range} = hdr_histogram:record(R, -1),
    {error, value_out_of_range} = hdr_histogram:record(R, 11).

t_issue_013(_Config) ->
    {ok,R} = hdr_histogram:open(10,1),
    [ begin
      ok = hdr_histogram:record_many(R, X, 10)
    end || X <- lists:seq(0,10) ],
    {error, value_out_of_range} = hdr_histogram:record(R, -1),
    {error, value_out_of_range} = hdr_histogram:record(R, 11).
    
t_unique_resource_types(_Config) ->
    {ok, H} = hdr_histogram:open(10, 1),
    {ok, I} = hdr_iter:open(record, H, []),
    try
        shouldnt_match = hdr_histogram:record(I, 1)
    catch
        error:badarg ->
            ok
    end.

step_counts() ->
    fun({_,Attrs},Acc) ->
        {step_count,X}=lists:keyfind(step_count,1,Attrs),
        Acc ++ [X]
    end.

accum_steps() ->
    fun({_,Attrs},Acc) ->
        {step_count,X}=lists:keyfind(step_count,1,Attrs),
        Acc + X
    end.

count() ->
    fun({_,_},Acc) ->
        Acc + 1
    end.

load_histograms() ->
    {ok,Raw} = hdr_histogram:open(3600 * 1000 * 1000, 3),
    {ok,Cor} = hdr_histogram:open(3600 * 1000 * 1000, 3),
    load(10000, {Raw,Cor}),
    ok = hdr_histogram:record(Raw, 100000000),
    ok = hdr_histogram:record_corrected(Cor,100000000,10000),
    {Raw,Cor}.
load(0,{Raw,Cor}) ->
    {Raw,Cor};
load(N,{Raw,Cor}) ->
    ok = hdr_histogram:record(Raw, 1000),
    ok = hdr_histogram:record_corrected(Cor,1000,10000),
    load(N-1,{Raw,Cor}).

cmp(L1,L2,D) ->
    case erlang:abs(L1-L2) < D of
	false -> throw({not_same, L1, L2, D, erlang:get_stacktrace()});
	true -> true
    end.
