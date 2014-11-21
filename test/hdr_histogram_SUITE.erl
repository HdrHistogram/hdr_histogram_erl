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

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, hdr}
    ].

groups() ->
    [{hdr, [], [
        t_hdr_create
      , t_hdr_invalid_sigfig
      , t_hdr_total_count
      , t_hdr_max_value
      , t_hdr_min_value
      , t_hdr_percentiles
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
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

t_hdr_create(_Config) ->
    {ok,R} = hdr_histogram:open(36000000, 4),
    1704008 = hdr_histogram:get_memory_size(R),
    ok.

t_hdr_invalid_sigfig(_Config) ->
    {error,bad_significant_factor} = (catch hdr_histogram:open(36000000, -1)),
    {error,bad_significant_factor} = (catch hdr_histogram:open(36000000, 6)),
    ok.

t_hdr_total_count(_Config) ->
    {Raw,Cor} = load_histograms(),
    10001 = hdr_histogram:get_total_count(Raw),
    20000 = hdr_histogram:get_total_count(Cor),
    ok.

t_hdr_max_value(_Config) ->
    {Raw,Cor} = load_histograms(),
    RawMax = hdr_histogram:max(Raw),
    CorMax = hdr_histogram:max(Cor),
    hdr_histogram:same(Raw,100000000,RawMax),
    hdr_histogram:same(Cor,100000000,CorMax),
    ok.

t_hdr_min_value(_Config) ->
    {Raw,Cor} = load_histograms(),
    1000 = hdr_histogram:min(Raw),
    1000 = hdr_histogram:min(Cor),
    ok.

t_hdr_percentiles(_Config) ->
    {Raw,Cor} = load_histograms(),
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
