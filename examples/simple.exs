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
