**hdr_histogram_erl** [![Build Status](https://travis-ci.org/HdrHistogram/hdr_histogram_erl.svg?branch=master)](https://travis-ci.org/HdrHistogram/hdr_histogram_erl) [![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/HdrHistogram/HdrHistogram?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

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

Enjoy!
