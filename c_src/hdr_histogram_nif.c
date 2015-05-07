// Copyright (c) 2014 Darach Ennis <darach@gmail.com>
//
// The HDR histogram library is an Erlang native interface function wrapper of
// Mike Barker's C port of Gil Tene's HDR Histogram utility.
//
// 
// A high dynamic range histogram is one that supports recording and analyzing
// sampled data points across a configurable range with configurable precision
// within that range. The precision is expressed as a number of significant
// figures in the recording.
//
// This HDR histogram implementation is designed for recording histograms of
// value measurements in latency sensitive environments. Although the native
// recording times can be as low as single digit nanoseconds there is added
// overhead in this wrapper/binding due to both the frontend overhead of converting
// from native C to the NIF interface, and the erlang overhead incurred calling
// into the NIFs. C'est la vie, I suppose.
//
// A distinct advantage of this histogram implementation is constant space and
// recording (time) overhead with an ability to recycle and reset instances whilst
// reclaiming already allocated space for reuse thereby reducing allocation cost
// and garbage collection overhead in the BEAM where repeated or continuous usage
// is likely. For example, a gen_server recording metrics continuously and resetting
// and logging histogram dumps on a periodic or other windowed basis.
//
// The code is released to the public domain, under the same terms as its
// sibling projects, as explained in the LICENSE.txt and COPYING.txt in the
// root of this repository, but normatively at:
//
// http://creativecommons.org/publicdomain/zero/1.0/
//
// For users of this code who wish to consume it under the "BSD" license
// rather than under the public domain or CC0 contribution text mentioned
// above, the code found under this directory is *also* provided under the
// following license (commonly referred to as the BSD 2-Clause License). This
// license does not detract from the above stated release of the code into
// the public domain, and simply represents an additional license granted by
// http://creativecommons.org/publicdomain/zero/1.0/
//
// -----------------------------------------------------------------------------
// ** Beginning of "BSD 2-Clause License" text. **
//
// Copyright (c) 2012, 2013, 2014 Gil Tene
// Copyright (c) 2014 Michael Barker
// Copyright (c) 2014 Darach Ennis
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
// THE POSSIBILITY OF SUCH DAMAGE.
//

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <math.h>
#include <errno.h>

#include "erl_nif.h"
#include "hdr_histogram.h"
#include "hdr_histogram_log.h"

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_BUCKET_IDX;
static ERL_NIF_TERM ATOM_COUNT_AT_IDX;
static ERL_NIF_TERM ATOM_HIGHEST_EQUIV_VAL;
static ERL_NIF_TERM ATOM_LINEAR;
static ERL_NIF_TERM ATOM_LINEAR_VAL_UNIT;
static ERL_NIF_TERM ATOM_LOGARITHMIC;
static ERL_NIF_TERM ATOM_LOG_BASE;
static ERL_NIF_TERM ATOM_LOG_VAL_UNIT;
static ERL_NIF_TERM ATOM_NEXT_VAL_REP_LVL;
static ERL_NIF_TERM ATOM_NEXT_VAL_REP_LVL_LOW_EQUIV;
static ERL_NIF_TERM ATOM_PERCENTILE;
static ERL_NIF_TERM ATOM_PERCENTILE_HALF_TICKS;
static ERL_NIF_TERM ATOM_PERCENTILE_TO_ITERATE_TO;
static ERL_NIF_TERM ATOM_RECORD;
static ERL_NIF_TERM ATOM_SEEN_LAST_VAL;
static ERL_NIF_TERM ATOM_STEP_COUNT;
static ERL_NIF_TERM ATOM_SUB_BUCKET_IDX;
static ERL_NIF_TERM ATOM_TICKS_PER_HALF_DISTANCE;
static ERL_NIF_TERM ATOM_VAL_AT_IDX;
static ERL_NIF_TERM ATOM_VAL_FROM_IDX;
static ERL_NIF_TERM ATOM_VAL_UNITS_FIRST_BUCKET;
static ERL_NIF_TERM ATOM_VAL_UNITS_PER_BUCKET;

typedef struct
{
    int64_t highest_trackable_value;
    int significant_figures;
    hdr_histogram_t* data;
} hh_ctx_t;

static inline ERL_NIF_TERM make_error(ErlNifEnv* env, const char* text)
{
    return enif_make_tuple2(
        env,
        ATOM_ERROR,
        enif_make_atom(env, text)
    );
}

static inline double round_to_significant_figures(double value, int figures)
{
    // This function does not handle values of 0 very well it will return inf
    // for the factor and return nan so we just make sure we handle 0 seperately.
    if (value == 0.)
    {
        return 0;
    }
    else
    {
        double factor = pow(10.0, figures - ceil(log10(fabs(value))));
        return round(value * factor) / factor;
    }
}

ERL_NIF_TERM _hh_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long highest_trackable_value = 0;
    int significant_figures = 0;
    if (argc != 2 ||
        !enif_get_int64(env, argv[0], &highest_trackable_value) ||
        !enif_get_int(env, argv[1], &significant_figures))
    {
        enif_make_badarg(env);
    }

    hdr_histogram_t* raw_histogram;

    int rc = 0;
    rc = hdr_alloc(highest_trackable_value, significant_figures, &raw_histogram);

    if (EINVAL == rc)
    {
        return make_error(env, "bad_significant_factor");
    }

    if (ENOMEM == rc)
    {
        return make_error(env, "not_enough_memory");
    }

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    hh_ctx_t* ctx = (hh_ctx_t*)enif_alloc_resource(ctx_type, sizeof(hh_ctx_t));

    ctx->data = raw_histogram; 
    ctx->highest_trackable_value = highest_trackable_value;
    ctx->significant_figures = significant_figures;

    ERL_NIF_TERM result = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, ATOM_OK, result);
}

ERL_NIF_TERM _hh_get_memory_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        return enif_make_ulong(env,hdr_get_memory_size(ctx->data));
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_get_total_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        return enif_make_ulong(env,ctx->data->total_count);
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_record(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long value = 0;
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if(argc != 2 || 
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value))
    {
        enif_make_badarg(env);
    }

    if (value < 0 || value > ctx->highest_trackable_value)
    {
	return make_error(env, "value_out_of_range");
    }

    if (ctx != NULL)
    {
        hdr_record_value(ctx->data, value);
    }

    return ATOM_OK;
}

ERL_NIF_TERM _hh_record_corrected(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long value = 0;
    long expected_interval = 0;
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if(argc != 3 ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value) ||
        !enif_get_int64(env, argv[2], &expected_interval))
    {
        enif_make_badarg(env);
    }


    if (value < 0 || value > ctx->highest_trackable_value)
    {
	    return make_error(env, "value_out_of_range");
    }

    if (ctx != NULL)
    {
        hdr_record_corrected_value(ctx->data, value, expected_interval);
    }

    return ATOM_OK;
}

ERL_NIF_TERM _hh_record_many(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long value = 0;
    long count = 0;
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if(argc != 3 ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value) ||
        !enif_get_int64(env, argv[2], &count))
    {
        enif_make_badarg(env);
    }

    if ( 
        value < 0 ||
	    value > ctx->data->highest_trackable_value)
    {
	    return make_error(env, "value_out_of_range");
    }

    if (ctx != NULL)
    {
        hdr_record_values(ctx->data, value, count);
    }

    return ATOM_OK;
}

ERL_NIF_TERM _hh_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;
    hh_ctx_t* from;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_resource(env, argv[1], ctx_type, (void **)&from))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        return enif_make_long(env, hdr_add(ctx->data, from->data));
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_min(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        return enif_make_long(env, hdr_min(ctx->data));
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        if (ctx->data->total_count == 0)
        {
            return enif_make_long(env, 0);
        }
        else
        {
            return enif_make_long(env, hdr_max(ctx->data));
        }
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_mean(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }
  
    if (ctx != NULL)
    {
        if (ctx->data->total_count == 0)
        {
            return enif_make_double(env, 0.);
        }
        else
        {
            return enif_make_double(
                env, 
                round_to_significant_figures(
                    hdr_mean(ctx->data),
                    ctx->significant_figures
                )
            );
        }
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_median(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        if (ctx->data->total_count == 0)
        {
            return enif_make_double(env, 0.0);
        }
        else
        {
            return enif_make_double(
                env,
                round_to_significant_figures(
                    hdr_value_at_percentile(ctx->data,50.0),
                    ctx->significant_figures
                )
            );
        }
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_stddev(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        if (ctx->data->total_count == 0)
        {
            return enif_make_double(env, 0.);
        }
        else
        {
            return enif_make_double(
                env,
                round_to_significant_figures(
                    hdr_stddev(ctx->data),
                    ctx->significant_figures
                )
            );
        }
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_percentile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double percentile;
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_double(env, argv[1], &percentile))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        if (ctx->data->total_count == 0)
        {
            return enif_make_double(env, 0.);
        }
        else
        {
            return enif_make_double(
                env,
                round_to_significant_figures(
                    hdr_value_at_percentile(ctx->data,percentile),
                    ctx->significant_figures
                )
            );
        }
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_same(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long a = 0;
    long b = 0;
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 3 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &a) ||
        !enif_get_int64(env, argv[2], &b))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        return hdr_values_are_equivalent(ctx->data,a,b)
            ? ATOM_TRUE : ATOM_FALSE;
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_lowest_at(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long value = 0;
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        return enif_make_long(
            env,
            hdr_lowest_equivalent_value(ctx->data, value)
        );
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_count_at(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    long value = 0;
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value))
    {
        return enif_make_badarg(env);
    }
   
    if (ctx != NULL)
    {
        return enif_make_long(env, hdr_count_at_value(ctx->data, value));
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_print_classic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        hdr_percentiles_print(ctx->data, stdout, 5, 1.0, CLASSIC);
        return ATOM_OK;
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_print_csv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        hdr_percentiles_print(ctx->data, stdout, 5, 1.0, CSV);
        return ATOM_OK;
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_log_classic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char fname[64];
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 || ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_string(env, argv[1], fname, 64, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    FILE* stream = fopen(fname,"w+");
    if (stream == NULL)
    {
        return make_error(env, "cannot_create_or_write_to_file");
    }

    if (ctx != NULL)
    {
        hdr_percentiles_print(ctx->data, stream, 5, 1.0, CLASSIC);
        if (fclose(stream) != 0)
        {
            return make_error(env, "bad_file");
        }
        return ATOM_OK;
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}


ERL_NIF_TERM _hh_log_csv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char fname[64];
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 || ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
    !enif_get_string(env, argv[1], fname, 64, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    FILE* stream = fopen(fname,"w+");
    if (stream == NULL)
    {
        return make_error(env, "cannot_create_or_write_to_file");
    }

    if (ctx != NULL)
    {
        hdr_percentiles_print(ctx->data, stream, 5, 1.0, CSV);
        if (fclose(stream) != 0)
        {
            return make_error(env, "bad_file");
        }
        return ATOM_OK;
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL && ctx->data != NULL)
    {
        hdr_reset(ctx->data);
        return ATOM_OK;
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL && ctx->data != NULL)
    {
        free(ctx->data);
        ctx->data = NULL;
    }
    enif_release_resource(ctx_type);

    return ATOM_OK;
}


ERL_NIF_TERM _hh_from_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary source;

    if (!enif_inspect_binary(env, argv[0], &source))
    {
      return enif_make_badarg(env);
    }

    hdr_histogram_t* target = NULL;
    int success = hdr_decode(source.data, source.size, &target);

    if (success != 0)
    {
        return make_error(env, "bad_hdr_binary");
    }

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    hh_ctx_t* ctx = (hh_ctx_t*)enif_alloc_resource(ctx_type, sizeof(hh_ctx_t));

    ctx->data = (hdr_histogram_t*)target;
    ctx->highest_trackable_value = target->highest_trackable_value;
    ctx->significant_figures = target->significant_figures;

    ERL_NIF_TERM result = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, ATOM_OK, result);
}

ERL_NIF_TERM _hh_to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;
    ErlNifBinary target;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))

    {
        return enif_make_badarg(env);
    }

    int size = 0;
    uint8_t* data = NULL;
    int success = hdr_encode_compressed(ctx->data, &data, &size);

    if (!enif_alloc_binary(size, &target))
    {
        return make_error(env, "bad_hdr_binary_alloc");
    }
    target.size = size;
    memcpy(target.data,data,size);
    free(data);

    if (success != 0)
    {
        return make_error(env, "bad_hdr_binary");
    }

    return enif_make_binary(env, &target);
}

ERL_NIF_TERM _hh_to_binary_uncompressed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx = NULL;
    ErlNifBinary target;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }
    int size = 0;
    uint8_t* data = NULL;
    int success = hdr_encode_uncompressed(ctx->data, &data, &size);

    if (!enif_alloc_binary(size, &target))
    {
        return make_error(env, "bad_hdr_binary_alloc");
    }
    target.size = size;
    memcpy(target.data,data,size);
    free(data);

    if (success != 0)
    {
        return make_error(env, "bad_hdr_binary");
    }

    return enif_make_binary(env, &target);
}

#define HDR_ITER_REC 1
#define HDR_ITER_LIN 2
#define HDR_ITER_LOG 4
#define HDR_ITER_PCT 8

typedef struct
{
    int linear_value_units_per_bucket;
    int log_value_units_first_bucket;
    double log_base;
    int percentile_ticks_per_half_distance;
} hi_opts_t;

typedef struct
{
    uint32_t type;
    hi_opts_t* opts;
    void* iter;
} hi_ctx_t;

ERL_NIF_TERM parse_opts(
    ErlNifEnv* env,
    ERL_NIF_TERM list,
    hi_opts_t* acc,
    ERL_NIF_TERM(*parse_opt)(ErlNifEnv*, ERL_NIF_TERM, void* acc)
)
{
    ERL_NIF_TERM hd, tl = list;
    while(enif_get_list_cell(env, tl, &hd, &tl))
    {
        ERL_NIF_TERM rc = parse_opt(env, hd, acc);
        if (rc != ATOM_OK)
        {
            return rc;
        }
    }

    return ATOM_OK;
}

ERL_NIF_TERM parse_opt(ErlNifEnv* env, ERL_NIF_TERM e, hi_opts_t* o)
{
    int arity;
    const ERL_NIF_TERM* opt;
    if (enif_get_tuple(env, e, &arity, &opt) && arity==2)
    {
        if (opt[0] == ATOM_LINEAR_VAL_UNIT)
        {
            uint32_t value_size;
            if (enif_get_uint(env, opt[1], &value_size))
            {
                o->linear_value_units_per_bucket = value_size;
            }
        }
        if (opt[0] == ATOM_LOG_VAL_UNIT)
        {
            uint32_t value_size;
            if (enif_get_uint(env, opt[1], &value_size))
            {
                o->log_value_units_first_bucket= value_size;
            }
        }
        if (opt[0] == ATOM_LOG_BASE)
        {
            double log_base;
            if (enif_get_double(env, opt[1], &log_base))
            {
                o->log_base = log_base;
            }
        }
        if (opt[0] == ATOM_PERCENTILE_HALF_TICKS)
        {
            uint32_t ticks_per_half;
            if (enif_get_uint(env, opt[1], &ticks_per_half))
            {
                o->percentile_ticks_per_half_distance = ticks_per_half;
            }
        }
    }

    return ATOM_OK;
}

ERL_NIF_TERM _hi_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32_t iterator_type = 0;

    if (argc != 1 ||
        !enif_get_uint(env, argv[0], &iterator_type))
    {
        enif_make_badarg(env);
    }

    if (iterator_type != HDR_ITER_REC &&
        iterator_type != HDR_ITER_LIN &&
        iterator_type != HDR_ITER_LOG &&
        iterator_type != HDR_ITER_PCT)
    {
        return make_error(env, "bad_iterator_type");
    }

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    hi_ctx_t* ctx = (hi_ctx_t*)enif_alloc_resource(ctx_type, sizeof(hi_ctx_t));

    ctx->type = iterator_type;
    ctx->opts = NULL;
    ctx->iter = NULL;

    ERL_NIF_TERM result = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, ATOM_OK, result);
}

ERL_NIF_TERM _hi_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hi_ctx_t* ctx = NULL;
    hh_ctx_t* hdr;
    hi_opts_t* opts;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 3 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_resource(env, argv[1], ctx_type, (void **)&hdr) ||
        !enif_is_list(env, argv[2]))
    {
        enif_make_badarg(env);
    }

    opts = (hi_opts_t *)enif_alloc(sizeof(hi_opts_t));
    parse_opts(env, argv[2], opts, (void *)parse_opt);
    uint32_t iterator_type = ctx->type;

    void* it = NULL;

    if (iterator_type == HDR_ITER_REC)
    {
        struct hdr_recorded_iter * iter =
            enif_alloc(sizeof(struct hdr_recorded_iter));
        hdr_recorded_iter_init(iter, hdr->data);
        it = iter;
    }

    if (iterator_type == HDR_ITER_LIN)
    {
        if (opts->linear_value_units_per_bucket <= 0)
        {
            return make_error(env, "bad_linear_value_unit");
        }
        struct hdr_linear_iter * iter =
            enif_alloc(sizeof(struct hdr_linear_iter));
        hdr_linear_iter_init(
            iter,
            hdr->data,
            opts->linear_value_units_per_bucket);
        it = iter;
    }

    if (iterator_type == HDR_ITER_LOG)
    {
        if (opts->log_value_units_first_bucket <= 0)
        {
            return make_error(env, "bad_log_value_unit");
        }
        if (opts->log_base <= 0)
        {
            return make_error(env, "bad_log_base");
        }
        struct hdr_log_iter * iter =
            enif_alloc(sizeof(struct hdr_log_iter));
        hdr_log_iter_init(
            iter,
            hdr->data,
            opts->log_value_units_first_bucket,
            opts->log_base);
        it = iter;
    }

    if (iterator_type == HDR_ITER_PCT)
    {
        if (opts->percentile_ticks_per_half_distance <= 0)
        {
            return make_error(env, "bad_percentile_half_ticks");
        }
        struct hdr_percentile_iter * iter =
            enif_alloc(sizeof(struct hdr_percentile_iter));
        hdr_percentile_iter_init(
            iter,
            hdr->data,
            opts->percentile_ticks_per_half_distance);
        it = iter;
    }

    ctx->type = iterator_type;
    ctx->opts = opts;
    ctx->iter = it;

    return ATOM_OK;
}

ERL_NIF_TERM _hi_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hi_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 ||
        ctx_type == NULL ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL && ctx->type == HDR_ITER_REC)
    {
        // skip until first non-zero index
        bool has_next = false;
        while (ctx->iter != NULL && (has_next = hdr_recorded_iter_next(ctx->iter)))
        {
            struct hdr_recorded_iter* x = ((struct hdr_recorded_iter*)ctx->iter);
            struct hdr_iter iter = x->iter;
            if (0 != iter.count_at_index)
            {
                int64_t stp = x->count_added_in_this_iteration_step;
                int32_t bdx = iter.bucket_index;
                int64_t sdx = iter.sub_bucket_index;
                int64_t val = iter.value_from_index;
                int64_t cat = iter.count_at_index;
                int64_t cto = iter.count_to_index;
                int64_t heq = iter.highest_equivalent_value;
                return enif_make_tuple2(env,
                  ATOM_RECORD,
                  enif_make_list(env, 7,
                      enif_make_tuple2(env,
                          ATOM_BUCKET_IDX,
                          enif_make_int(env,bdx)),
                      enif_make_tuple2(env,
                          ATOM_SUB_BUCKET_IDX,
                          enif_make_long(env,sdx)),
                      enif_make_tuple2(env,
                          ATOM_VAL_FROM_IDX,
                          enif_make_long(env,val)),
                      enif_make_tuple2(env,
                          ATOM_VAL_AT_IDX,
                          enif_make_long(env,cat)),
                      enif_make_tuple2(env,
                          ATOM_COUNT_AT_IDX,
                          enif_make_long(env,cto)),
                      enif_make_tuple2(env,
                          ATOM_HIGHEST_EQUIV_VAL,
                          enif_make_long(env,heq)),
                      enif_make_tuple2(env,
                          ATOM_STEP_COUNT,
                          enif_make_long(env,stp))));
            }
        }
    }

    if (ctx != NULL && ctx->type == HDR_ITER_LIN)
    {
        // skip until first non-zero index
        bool has_next = false;
        while (ctx->iter != NULL && (has_next = hdr_linear_iter_next(ctx->iter)))
        {
            struct hdr_linear_iter* x = ((struct hdr_linear_iter*)ctx->iter);
            struct hdr_iter iter = x->iter;
            if (0 != iter.count_at_index)
            {
                int64_t stp = x->count_added_in_this_iteration_step;
                int32_t vub = x->value_units_per_bucket;
                int64_t nvl = x->next_value_reporting_level;
                int64_t nve = x->next_value_reporting_level_lowest_equivalent;
                int32_t bdx = iter.bucket_index;
                int64_t sdx = iter.sub_bucket_index;
                int64_t val = iter.value_from_index;
                int64_t cat = iter.count_at_index;
                int64_t cto = iter.count_to_index;
                int64_t heq = iter.highest_equivalent_value;
                return enif_make_tuple2(env,
                  ATOM_LINEAR,
                  enif_make_list(env, 10,
                      enif_make_tuple2(env,
                          ATOM_BUCKET_IDX,
                          enif_make_int(env,bdx)),
                      enif_make_tuple2(env,
                          ATOM_SUB_BUCKET_IDX,
                          enif_make_long(env,sdx)),
                      enif_make_tuple2(env,
                          ATOM_VAL_FROM_IDX,
                          enif_make_long(env,val)),
                      enif_make_tuple2(env,
                          ATOM_VAL_AT_IDX,
                          enif_make_long(env,cat)),
                      enif_make_tuple2(env,
                          ATOM_COUNT_AT_IDX,
                          enif_make_long(env,cto)),
                      enif_make_tuple2(env,
                          ATOM_HIGHEST_EQUIV_VAL,
                          enif_make_long(env,heq)),
                      enif_make_tuple2(env,
                          ATOM_VAL_UNITS_PER_BUCKET,
                          enif_make_int(env,vub)),
                      enif_make_tuple2(env,
                          ATOM_STEP_COUNT,
                          enif_make_long(env,stp)),
                      enif_make_tuple2(env,
                          ATOM_NEXT_VAL_REP_LVL,
                          enif_make_long(env,nvl)),
                      enif_make_tuple2(env,
                          ATOM_NEXT_VAL_REP_LVL_LOW_EQUIV,
                          enif_make_long(env,nve))));
            }
        }
    }

    if (ctx != NULL && ctx->type == HDR_ITER_LOG)
    {
        // skip until first non-zero index
        bool has_next = false;
        while (ctx->iter != NULL && (has_next = hdr_log_iter_next(ctx->iter)))
        {
            struct hdr_log_iter* x = (struct hdr_log_iter*)ctx->iter;
            struct hdr_iter iter = x->iter;
            if (0 != iter.count_at_index)
            {
                int64_t stp = x->count_added_in_this_iteration_step;
                int32_t vfb = x->value_units_first_bucket;
                double lgb = x->log_base;
                int64_t nvl = x->next_value_reporting_level;
                int64_t nve = x->next_value_reporting_level_lowest_equivalent;
                int32_t bdx = iter.bucket_index;
                int64_t sdx = iter.sub_bucket_index;
                int64_t val = iter.value_from_index;
                int64_t cat = iter.count_at_index;
                int64_t cto = iter.count_to_index;
                int64_t heq = iter.highest_equivalent_value;
                return enif_make_tuple2(env,
                  ATOM_LOGARITHMIC,
                  enif_make_list(env, 11,
                      enif_make_tuple2(env,
                          ATOM_BUCKET_IDX,
                          enif_make_int(env,bdx)),
                      enif_make_tuple2(env,
                          ATOM_SUB_BUCKET_IDX,
                          enif_make_long(env,sdx)),
                      enif_make_tuple2(env,
                          ATOM_VAL_FROM_IDX,
                          enif_make_long(env,val)),
                      enif_make_tuple2(env,
                          ATOM_VAL_AT_IDX,
                          enif_make_long(env,cat)),
                      enif_make_tuple2(env,
                          ATOM_COUNT_AT_IDX,
                          enif_make_long(env,cto)),
                      enif_make_tuple2(env,
                          ATOM_HIGHEST_EQUIV_VAL,
                          enif_make_long(env,heq)),
                      enif_make_tuple2(env,
                          ATOM_VAL_UNITS_FIRST_BUCKET,
                          enif_make_int(env,vfb)),
                      enif_make_tuple2(env,
                          ATOM_STEP_COUNT,
                          enif_make_long(env,stp)),
                      enif_make_tuple2(env,
                          ATOM_LOG_BASE,
                          enif_make_double(env,lgb)),
                      enif_make_tuple2(env,
                          ATOM_NEXT_VAL_REP_LVL,
                          enif_make_long(env,nvl)),
                      enif_make_tuple2(env,
                          ATOM_NEXT_VAL_REP_LVL_LOW_EQUIV,
                          enif_make_long(env,nve))));
            }
        }
    }

    if (ctx != NULL && ctx->type == HDR_ITER_PCT)
    {
        // skip until first non-zero index
        bool has_next = false;
        while (ctx->iter != NULL && (has_next = hdr_percentile_iter_next(ctx->iter)))
        {
            struct hdr_percentile_iter* x = (struct hdr_percentile_iter*)ctx->iter;
            struct hdr_iter iter = x->iter;
            if (0 != iter.count_at_index)
            {
                bool slv = x->seen_last_value;
                int32_t tph = x->ticks_per_half_distance;
                double pti = x->percentile_to_iterate_to;
                double pct = x->percentile;
                int32_t bdx = iter.bucket_index;
                int64_t sdx = iter.sub_bucket_index;
                int64_t val = iter.value_from_index;
                int64_t cat = iter.count_at_index;
                int64_t cto = iter.count_to_index;
                int64_t heq = iter.highest_equivalent_value;
                return enif_make_tuple2(env,
                  ATOM_PERCENTILE,
                  enif_make_list(env, 10,
                      enif_make_tuple2(env,
                          ATOM_BUCKET_IDX,
                          enif_make_int(env,bdx)),
                      enif_make_tuple2(env,
                          ATOM_SUB_BUCKET_IDX,
                          enif_make_long(env,sdx)),
                      enif_make_tuple2(env,
                          ATOM_VAL_FROM_IDX,
                          enif_make_long(env,val)),
                      enif_make_tuple2(env,
                          ATOM_VAL_AT_IDX,
                          enif_make_long(env,cat)),
                      enif_make_tuple2(env,
                          ATOM_COUNT_AT_IDX,
                          enif_make_long(env,cto)),
                      enif_make_tuple2(env,
                          ATOM_HIGHEST_EQUIV_VAL,
                          enif_make_long(env,heq)),
                      enif_make_tuple2(env,
                          ATOM_SEEN_LAST_VAL,
                          enif_make_atom(env,slv ? "true" : "false")),
                      enif_make_tuple2(env,
                          ATOM_PERCENTILE_TO_ITERATE_TO,
                          enif_make_double(env,pti)),
                      enif_make_tuple2(env,
                          ATOM_PERCENTILE,
                          enif_make_double(env,pct)),
                      enif_make_tuple2(env,
                          ATOM_TICKS_PER_HALF_DISTANCE,
                          enif_make_int(env,tph))));
            }
        }
    }

    return enif_make_tuple2(
        env,
        ATOM_FALSE,
        enif_make_tuple(env,0)
    );
}

ERL_NIF_TERM _hi_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hi_ctx_t* ctx = NULL;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL && ctx->opts != NULL)
    {
        enif_free(ctx->opts);
        enif_free(ctx->iter);
        ctx->opts = NULL;
        ctx->iter = NULL;
    }
    enif_release_resource(ctx_type);

    return ATOM_OK;
}

static void init(ErlNifEnv* env)
{
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_BUCKET_IDX = enif_make_atom(env, "bucket_index");
    ATOM_COUNT_AT_IDX = enif_make_atom(env, "count_at_index");
    ATOM_HIGHEST_EQUIV_VAL = enif_make_atom(env, "highest_equivalent_value");
    ATOM_LINEAR = enif_make_atom(env, "linear");
    ATOM_LINEAR_VAL_UNIT = enif_make_atom(env, "linear_value_unit");
    ATOM_LOGARITHMIC = enif_make_atom(env, "logarithmic");
    ATOM_LOG_BASE = enif_make_atom(env, "log_base");
    ATOM_LOG_VAL_UNIT = enif_make_atom(env, "log_value_unit");
    ATOM_NEXT_VAL_REP_LVL = enif_make_atom(env, "next_value_reporting_level");
    ATOM_NEXT_VAL_REP_LVL_LOW_EQUIV = enif_make_atom(env, "next_value_reporting_level_lowest_equiv");
    ATOM_PERCENTILE = enif_make_atom(env, "percentile");
    ATOM_PERCENTILE_HALF_TICKS = enif_make_atom(env, "percentile_half_ticks");
    ATOM_PERCENTILE_TO_ITERATE_TO = enif_make_atom(env, "percentile_to_iterate_to");
    ATOM_RECORD = enif_make_atom(env, "record");
    ATOM_SEEN_LAST_VAL = enif_make_atom(env, "seen_last_value");
    ATOM_STEP_COUNT = enif_make_atom(env, "step_count");
    ATOM_SUB_BUCKET_IDX = enif_make_atom(env, "sub_bucket_index");
    ATOM_TICKS_PER_HALF_DISTANCE = enif_make_atom(env, "ticks_per_half_distance");
    ATOM_VAL_AT_IDX = enif_make_atom(env, "value_at_index");
    ATOM_VAL_FROM_IDX = enif_make_atom(env, "value_from_index");
    ATOM_VAL_UNITS_FIRST_BUCKET = enif_make_atom(env, "value_units_first_bucket");
    ATOM_VAL_UNITS_PER_BUCKET = enif_make_atom(env, "value_units_per_bucket");
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    init(env);
    *priv_data = enif_open_resource_type(env, NULL, "hh_ctx_t", NULL, ERL_NIF_RT_CREATE, NULL);
    return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    init(env);
    return 0;
}

static void on_unload(ErlNifEnv* env, void *priv_data)
{
    if (priv_data != NULL)
    {
       free(priv_data);
    }
}

static ErlNifFunc nif_funcs[] =
{
    // HDR histogram core
    {"open", 2, _hh_open},
    {"get_memory_size", 1, _hh_get_memory_size},
    {"get_total_count", 1, _hh_get_total_count},
    {"record", 2, _hh_record},
    {"record_corrected" , 3, _hh_record_corrected},
    {"record_many", 3, _hh_record_many},
    {"add", 2, _hh_add},
    {"min", 1, _hh_min},
    {"max", 1, _hh_max},
    {"mean", 1, _hh_mean},
    {"median", 1, _hh_median},
    {"stddev", 1, _hh_stddev},
    {"percentile", 2, _hh_percentile},
    {"same", 3, _hh_same},
    {"lowest_at", 2, _hh_lowest_at},
    {"count_at", 2, _hh_count_at},
    {"print_classic", 1, _hh_print_classic},
    {"print_csv", 1, _hh_print_csv},
    {"log_classic", 2, _hh_log_classic},
    {"log_csv", 2, _hh_log_csv},
    {"reset", 1, _hh_reset},
    {"close", 1, _hh_close},
    {"from_binary", 1, _hh_from_binary},
    {"to_binary", 1, _hh_to_binary},
    {"to_binary_uncompressed", 1, _hh_to_binary_uncompressed},
    // HDR histogram iteration facility
    {"iter_open", 1, _hi_open},
    {"iter_init", 3, _hi_init},
    {"iter_next", 1, _hi_next},
    {"iter_close", 1, _hi_close}
};

ERL_NIF_INIT(hdr_histogram, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
