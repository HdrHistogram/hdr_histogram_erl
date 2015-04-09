**hdr_histogram_erl** [![Build Status](https://travis-ci.org/HdrHistogram/hdr_histogram_erl.svg?branch=master)](https://travis-ci.org/HdrHistogram/hdr_histogram_erl) [![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/HdrHistogram/HdrHistogram?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [<img src="http://quickcheck-ci.com/p/hdrhistogram/hdr_histogram_erl.png" alt="Build Status" width="100px" height="18px">](http://quickcheck-ci.com/p/hdrhistogram/hdr_histogram_erl)

## Description ##

The HDR histogram library is an Erlang native interface function wrapper of
Mike Barker's C port of Gil Tene's HDR Histogram utility.

A high dynamic range histogram is one that supports recording and analyzing
sampled data points across a configurable range with configurable precision
within that range. The precision is expressed as a number of significant
figures in the recording.


This HDR histogram implementation is designed for recording histograms of
value measurements in latency sensitive environments. Although the native
recording times can be as low as single digit nanoseconds there is added
overhead in this wrapper/binding due to both the frontend overhead of converting
from native C to the NIF interface, and the erlang overhead incurred calling
into the NIFs. C'est la vie, I suppose.


A distinct advantage of this histogram implementation is constant space and
recording (time) overhead with an ability to recycle and reset instances whilst
reclaiming already allocated space for reuse thereby reducing allocation cost
and garbage collection overhead in the BEAM where repeated or continuous usage
is likely. For example, a gen_server recording metrics continuously and resetting
and logging histogram dumps on a periodic or other windowed basis.



The code is released to the public domain, under the same terms as its
sibling projects, as explained in the LICENSE.txt and COPYING.txt in the
root of this repository, but normatively at:

http://creativecommons.org/publicdomain/zero/1.0/

### Examples

Capture metrics and produce a histogram using Erlang/OTP:


```erlang
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
```

The same library works with other BEAM hosted languages, such as Elixir:

```elixir
#
# Simple histogram capture example using Elixir
#

defmodule Simple do
    def main do
        # Create a fresh HDR histogram instance
            {:ok,r} = :hdr_histogram.open(10000000,3)

            # record a random uniform distribution of 1M data points
        for n <- 1..1000000, do: :hdr_histogram.record(r,:random.uniform(n))

        # print percentiles to stdout as CLASSIC
        :hdr_histogram.print(r,:classic)

        # log percentiles to file as CSV
        # ELIXIR BUG :hdr_histogram.log(r,:csv,"elixir.hgrm")

        # print other values
        IO.puts "Min #{:hdr_histogram.min(r)}"
        IO.puts "Mean #{:hdr_histogram.mean(r)}"
        IO.puts "Median #{:hdr_histogram.median(r)}"
        IO.puts "Max #{:hdr_histogram.max(r)}"
        IO.puts "Stddev #{:hdr_histogram.stddev(r)}"
        IO.puts "99ile #{:hdr_histogram.percentile(r,99.0)}"
        IO.puts "99.9999ile #{:hdr_histogram.percentile(r,99.9999)}"
        IO.puts "Memory Size #{:hdr_histogram.get_memory_size(r)}"
        IO.puts "Total Count #{:hdr_histogram.get_total_count(r)}"

        # we're done, cleanup any held resources
        :hdr_histogram.close(r)

        IO.puts "Done!"
        end
end
```

[API documentation](doc/README.md)

## A note on tuning ##


A common useage example of HdrHistogram is to record response times, in units ofmicroseconds, across a dynamic range stretching from 1 usec to over an hour. We want a good enough resolution to support performing post-recording analysis on the collected data at some future time.

In order to facilitate the accuracy needed for such post-recording activities, we can maintain a resolution of ~1 usec or better for times ranging to ~2 msec in magnitude, while at the same time maintaining a resolution of ~1 msec or better for times ranging to ~2 sec, and a resolution of ~1 second or better for values up to 2,000 seconds, and so on. This sort of dynamic resolution can be thought of as "always accurate to 3 decimal points".

A HDR Histogram works like this. We MUST tune the highest trackable value to 3,600,000,000, and the number of significant value digits of 3. This range is fixed, and occupies a fixed, unchanging memory footprint of around 185KB.

## A note on footprint estimation ##


Due to it's **dynamic range** representation, HDR Histogram is relatively efficient in memory space requirements given the accuracy and dynamic range that it covers.

Still, it is useful to be able to estimate the memory footprint involved for a given highest trackable value and the configured number of significant value digits combination. Beyond a relatively small fixed-size footprint used for internal fields and stats (which can be estimated as "fixed at well less than 1KB"), the bulk of a histogram's storage is taken up by it's data value recording counts array. The total footprint can be conservatively estimated by:

```
 largestValueWithSingleUnitResolution =
        2 * (10 ^ numberOfSignificantValueDigits);
 subBucketSize =
        roundedUpToNearestPowerOf2(largestValueWithSingleUnitResolution);

 expectedHistogramFootprintInBytes = 512 +
      ({primitive type size} / 2) *
      (log2RoundedUp((highestTrackableValue) / subBucketSize) + 2) *
      subBucketSize
```

A conservative (high) estimate of a Histogram's footprint in bytes is available via thegetEstimatedFootprintInBytes() method.

## A note on concurrent access ##

HdrHistogram does *NOT* have any internal synchronization and at present hdr_histogram_erl does *NOT* provide any synchronization. This means that a Histogram reference must not be written to or read from multiple processes. 

It is recommended that you either wrap an hdr_histogram in a process, thus serializing access. It is also possible to use `hdr_histogram:add/2` to aggregate the contents of two histograms, making it possible to utilize per-process histograms that are aggregated by a separate reporting process.

## Enjoy!
