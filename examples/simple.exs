# Run with elixir -pa ./ebin examples/simple.exs

#
# Simple histogram capture example using Elixir
#
defmodule Simple do
  def main do
    # create a fresh HDR histogram instance
    {:ok, ref} = :hdr_histogram.open(1000000, 3)

    n = 10000000

    # record a random uniform distribution of 1M data points
    started = :os.timestamp
    loop(ref, n)
    ended = :os.timestamp

    duration = :timer.now_diff(ended, started) / 1.0e6
    rate = case duration > 1 do
       true -> n/duration
       false -> n*duration
    end
    :io.format("Runtime: ~psecs ~.5frps~n", [duration, rate])

    # print percentiles to stdout as CSV
    :hdr_histogram.print(ref, :csv)

    # log percentiles to file as CLASSIC
    :hdr_histogram.log(ref, :classic, 'elixir.hgrm')

    # print other values
    IO.puts "Min #{:hdr_histogram.min(ref)}"
    IO.puts "Mean #{:hdr_histogram.mean(ref)}"
    IO.puts "Median #{:hdr_histogram.median(ref)}"
    IO.puts "Max #{:hdr_histogram.max(ref)}"
    IO.puts "Stddev #{:hdr_histogram.stddev(ref)}"
    IO.puts "99ile #{:hdr_histogram.percentile(ref,99.0)}"
    IO.puts "99.9999ile #{:hdr_histogram.percentile(ref,99.9999)}"
    IO.puts "Memory Size #{:hdr_histogram.get_memory_size(ref)}"
    IO.puts "Total Count #{:hdr_histogram.get_total_count(ref)}"

    # we're done, cleanup any held resources
    :hdr_histogram.close(ref)

    IO.puts "Done!"
  end

  def loop(_ref, 0), do: :ok
  def loop(ref, x) do
    :hdr_histogram.record(ref, :random.uniform(1000000))
    loop(ref, x - 1)
  end
end

Simple.main
