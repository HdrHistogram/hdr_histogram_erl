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

-module(hdr_iter).

-export([open/3]).
-export([each/2]).
-export([each/3]).
-export([close/1]).

-type ref() :: binary().  %% NIF private data (looks like empty binary)

-spec open(IterationType, HdrRef, Options)
    -> {ok,Ref} | {error,Reason} when
    IterationType :: record | linear | logarithmic | percentile,
    HdrRef :: ref(),
    Ref :: ref(),
    Options :: [proplists:property()],
    Reason :: term().
%% @doc Open a fresh instance of a high dynamic range (HDR) histogram iterator
open(record,HdrRef,Options) ->
    open_and_init(1, HdrRef, Options);
open(linear,HdrRef,Options) ->
    open_and_init(2, HdrRef, Options);
open(logarithmic,HdrRef,Options) ->
    open_and_init(4, HdrRef, Options);
open(percentile,HdrRef,Options) ->
    open_and_init(8, HdrRef, Options).

%% @private
open_and_init(IterType,HdrRef,Options) ->
    {ok,IterRef} = hdr_histogram:iter_open(IterType),
    case hdr_histogram:iter_init(IterRef,HdrRef,Options) of
        ok -> {ok,IterRef};
        Error -> throw({bad_init,Error})
    end.

-spec each(IteratorRef,EachFun) -> ok | {error,Reason} when
    EachFun :: fun((Data) -> any()),
    IteratorRef :: ref(),
    Data :: term(),
    Reason :: term().
%% @doc Iterate over histogram applying a function to each data point 
each(IteratorRef,EachFun) ->
    each(IteratorRef,fun(Data,_) -> 
        EachFun(Data),
        ok 
    end, ok).

-spec each(IteratorRef,EachFun,Initial) -> {ok,Accum} | {error,Reason} when
    EachFun :: fun(({IteratorType,Data},Acc) -> any()),
    IteratorType :: record | linear | logarithmic | percentile,
    Data :: list({atom(),term()}), %% TODO FIXME type spec
    Acc :: term(),
    IteratorRef :: ref(),
    Reason :: term(),
    Initial :: term(),
    Accum :: term().
%% @doc Iterate over histogram applying a function to each data point accumulating a result
each(IteratorRef,EachFun,InitialAcc) ->
    scan(IteratorRef,EachFun,InitialAcc).

scan(IteratorRef,EachFun,Acc) ->
    case hdr_histogram:iter_next(IteratorRef) of
        {false,_} -> Acc;
        {Type,Data} ->
            NewAcc = EachFun({Type,Data},Acc),
            scan(IteratorRef,EachFun,NewAcc)
    end.

-spec close(Ref) -> ok | {error,term()} when
    Ref :: ref().
%% @doc Close this HDR histogram instance and free any system resources
close(Ref) ->
    hdr_histogram:iter_close(Ref).
