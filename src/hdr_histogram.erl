%% @author Darach Ennis <darach@gmail.com>
%% @copyright 2014 Darach Ennis
%% @version 0.2.0
%%
%% @doc
%% 
%% The HDR histogram library is an Erlang native interface function wrapper of
%% Mike Barker's C port of Gil Tene's HDR Histogram utility.
%%
%% 
%% A high dynamic range histogram is one that supports recording and analyzing
%% sampled data points across a configurable range with configurable precision
%% within that range. The precision is expressed as a number of significant
%% figures in the recording.
%%
%% This HDR histogram implementation is designed for recording histograms of
%% value measurements in latency sensitive environments. Although the native
%% recording times can be as low as single digit nanoseconds there is added
%% overhead in this wrapper/binding due to both the frontend overhead of converting
%% from native C to the NIF interface, and the erlang overhead incurred calling
%% into the NIFs. C'est la vie, I suppose.
%%
%% A distinct advantage of this histogram implementation is constant space and
%% recording (time) overhead with an ability to recycle and reset instances whilst
%% reclaiming already allocated space for reuse thereby reducing allocation cost
%% and garbage collection overhead in the BEAM where repeated or continuous usage
%% is likely. For example, a gen_server recording metrics continuously and resetting
%% and logging histogram dumps on a periodic or other windowed basis.
%%
%% The code is released to the public domain, under the same terms as its
%% sibling projects, as explained in the LICENSE.txt and COPYING.txt in the
%% root of this repository, but normatively at:
%%
%% http://creativecommons.org/publicdomain/zero/1.0/
%%
%% For users of this code who wish to consume it under the "BSD" license
%% rather than under the public domain or CC0 contribution text mentioned
%% above, the code found under this directory is *also* provided under the
%% following license (commonly referred to as the BSD 2-Clause License). This
%% license does not detract from the above stated release of the code into
%% the public domain, and simply represents an additional license granted by
%% http://creativecommons.org/publicdomain/zero/1.0/
%%
%% -----------------------------------------------------------------------------
%% ** Beginning of "BSD 2-Clause License" text. **
%%
%% Copyright (c) 2012, 2013, 2014 Gil Tene
%% Copyright (c) 2014 Michael Barker
%% Copyright (c) 2014 Darach Ennis
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% 1. Redistributions of source code must retain the above copyright notice,
%% this list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
%% THE POSSIBILITY OF SUCH DAMAGE.
%%
%% @end

-module(hdr_histogram).

-export([open/2]).
-export([get_memory_size/1]).
-export([get_total_count/1]).
-export([record/2]).
-export([record_corrected/3]).
-export([record_many/3]).
-export([add/2]).
-export([min/1]).
-export([max/1]).
-export([mean/1]).
-export([median/1]).
-export([stddev/1]).
-export([percentile/2]).
-export([same/3]).
-export([lowest_at/2]).
-export([count_at/2]).
-export([print/2]).
-export([log/3]).
-export([reset/1]).
-export([close/1]).
-export([from_binary/1]).
-export([to_binary/1]).
-export([to_binary/2]).
-export([iter_open/1]).
-export([iter_init/3]).
-export([iter_next/1]).
-export([iter_close/1]).

-on_load(init/0).

-type ref() :: binary().  %% NIF private data (looks like empty binary)

init() ->
    SoName = filename:join(
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                Dir = code:which(?MODULE),
                filename:join([filename:dirname(Dir),"..","priv"]);
            Dir -> Dir
        end, atom_to_list(?MODULE) ++ "_nif"),
    erlang:load_nif(SoName, 0).

-spec open(HighestTrackableValue,SignificantFigures)
    -> {ok,Ref} | {error,Reason} when
    HighestTrackableValue :: integer(),
    SignificantFigures :: 1..5,
    Ref :: ref(),
    Reason :: term().
%% @doc Open a fresh instance of a high dynamic range (HDR) histogram
open(_High,_Sig) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec get_memory_size(Ref) -> Size when
    Ref :: ref(),
    Size :: integer().
%% @doc Get memory footprint of an HDR histogram
get_memory_size(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec get_total_count(Ref) -> Count when
    Ref :: ref(),
    Count :: integer().
%% @doc Get total count of record values
get_total_count(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec record(Ref,Value) -> ok | {error, Reason} when
    Ref :: ref(),
    Value :: integer(),
    Reason :: term().
%% @doc Record an uncorrected histogram data point value in a HDR histogram
record(_Ref,_Value) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec record_corrected(Ref,Value,ExpectedInterval) -> ok | {error, Reason} when
    Ref :: ref(),
    Value :: integer(),
    ExpectedInterval :: integer(),
    Reason :: term().
%% @doc Record a histogram data point value in a HDR histogram with
%% expected interval for correction for coordinated ommission
record_corrected(_Ref,_Value,_ExpectedInterval) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec record_many(Ref,Value,Count) -> ok | {error, Reason} when
    Ref :: ref(),
    Value :: integer(),
    Count :: integer(),
    Reason :: term().
%% @doc Record a histogram data point value and number of occurances
record_many(_Ref,_Value,_Count) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec add(To,From) -> ok | {error,Reason}  when
    To :: ref(),
    From :: ref(),
    Reason :: term().
%% @doc Contribute the data points from a HDR histogram to another
add(_ToRef,_FromRef) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec min(Ref) -> MinValue when
    Ref :: ref(),
    MinValue :: integer().
%% @doc Get the minimum recorded data point value
min(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec max(Ref) -> MaxValue when
    Ref :: ref(),
    MaxValue :: integer().
%% @doc Get the maximum recorded data point value
max(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec mean(Ref) -> MeanValue when
    Ref :: ref(),
    MeanValue :: float().
%% @doc Get the mean data point value to a significant figure
mean(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec median(Ref) -> MedianValue when
    Ref :: ref(),
    MedianValue :: float().
%% @doc Get the median data point value to a significant figure
median(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec stddev(Ref) -> StddevValue when
    Ref :: ref(),
    StddevValue :: float().
%% @doc Get the standard deviation data point value to a significant figure
stddev(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec percentile(Ref,Percentile) -> PercentileValue when
    Ref :: ref(),
    Percentile :: float(),
    PercentileValue :: float().
%% @doc Get the specified percentile  data point value to a significant figure
percentile(_Ref,_Percentile) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec same(Ref,A,B) -> AreEquivalent when
    Ref :: ref(),
    A :: integer(),
    B :: integer(),
    AreEquivalent :: boolean().
%% @doc Are two data point values considered to be equivalent
same(_Ref,_A,_B) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec lowest_at(Ref,Value) -> LowestValueAt when
    Ref :: ref(),
    Value :: integer(),
    LowestValueAt :: float().
%% @doc Get the lowest equivalent value
lowest_at(_Ref,_Value) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec count_at(Ref,Value) -> CountAt when
    Ref :: ref(),
    Value :: integer(),
    CountAt :: integer().
%% @doc Get the count of values at a given at a given value
count_at(_Ref,_Value) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec print(Ref,Format) -> ok | {error, Reason} when
    Ref :: ref(),
    Format :: classic | csv,
    Reason :: term().
%% @doc Print the histogram to standard output in classic or CSV format
print(Ref,classic) ->
    print_classic(Ref);
print(Ref,csv) ->
    print_csv(Ref).

%% @private
print_classic(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @private
print_csv(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec log(Ref,Format,file:name()) -> ok | {error, Reason} when
    Ref :: ref(),
    Format :: classic | csv,
    Reason :: term().
%% @doc Log the histogram to a file in classic or CSV format
log(Ref,classic,FileName) ->
    log_classic(Ref,FileName);
log(Ref,csv,FileName) ->
    log_csv(Ref,FileName).

%% @private
log_classic(_Ref,_FileName) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @private
log_csv(_Ref,_FileName) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec reset(Ref) -> ok | {error,term()} when
    Ref :: ref().
%% @doc Reset the memory backing this HDR histogram instance and zero results
reset(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec close(Ref) -> ok | {error,term()} when
    Ref :: ref().
%% @doc Close this HDR histogram instance and free any system resources
close(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec from_binary(Binary) -> {ok,Ref} | {error,term()} when
    Binary :: binary(),
    Ref :: ref().
%% @doc Take a snapshot of HDR histogram internal state as a compressed binary and hydrate/open a reference. The reference SHOULD be closed when no longer needed to reclaim the memory used 
from_binary(_Binary) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec to_binary(Ref) -> binary() | {error,term()} when
    Ref :: ref().
%% @doc Take a snapshot of HDR histogram internal state as a compressed binary. The reference HDR instance can be modified after a snapshot is taken in the usual way with no restrictions.
to_binary(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec to_binary_uncompressed(Ref) -> binary() | {error,term()} when
    Ref :: ref().
to_binary_uncompressed(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec to_binary(Ref, [{compression, none} |
                      {compression, zlib}]) ->
                       binary() | {error,term()} when
      Ref :: ref().

to_binary(Ref, []) ->
    to_binary(Ref);
to_binary(Ref, [{compression, zlib}]) ->
    to_binary(Ref);
to_binary(Ref, [{compression, none}]) ->
    to_binary_uncompressed(Ref);
to_binary(_Ref, _) ->
    {error, bad_options}.

%% @private
iter_open(_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @private
iter_init(_,_,_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @private
iter_next(_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%% @private
iter_close(_) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
