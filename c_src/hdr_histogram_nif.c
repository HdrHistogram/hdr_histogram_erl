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
#include <math.h>
#include <errno.h>

#include "erl_nif.h"
#include "hdr_histogram.h" 

static inline ERL_NIF_TERM make_error(ErlNifEnv* env, const char* text)
{
    return enif_make_tuple2(
        env,
        enif_make_atom(env, "error"),
        enif_make_atom(env, text)
    );
}

static inline double round_to_significant_figures(double value, int figures)
{
    double factor = pow(10.0, figures - ceil(log10(fabs(value))));
    return round(value * factor) / factor;  
}

typedef struct
{
    int64_t highest_trackable_value;
    int significant_figures;
    hdr_histogram_t* data;
} hh_ctx_t;

ERL_NIF_TERM _hh_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int64_t highest_trackable_value;
    int significant_figures;
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
    enif_keep_resource(ctx);

    ctx->data = raw_histogram; 
    ctx->highest_trackable_value = highest_trackable_value;
    ctx->significant_figures = significant_figures;

    ERL_NIF_TERM result = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

ERL_NIF_TERM _hh_get_memory_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx;

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
    hh_ctx_t* ctx;

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
    int64_t value;
    hh_ctx_t* ctx;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if(argc != 2 || 
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value))
    {
        enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        hdr_record_value(ctx->data, value);
    }

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM _hh_record_corrected(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int64_t value;
    int64_t expected_interval;
    hh_ctx_t* ctx;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if(argc != 3 ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value) ||
        !enif_get_int64(env, argv[2], &expected_interval))
    {
        enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        hdr_record_corrected_value(ctx->data, value, expected_interval);
    }

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM _hh_record_many(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int64_t value;
    int64_t count;
    hh_ctx_t* ctx;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if(argc != 3 ||
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx) ||
        !enif_get_int64(env, argv[1], &value) ||
        !enif_get_int64(env, argv[2], &count))
    {
        enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        hdr_record_values(ctx->data, value, count);
    }

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM _hh_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx;
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
    hh_ctx_t* ctx;

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
    hh_ctx_t* ctx;

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
    hh_ctx_t* ctx;

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
    hh_ctx_t* ctx;

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
    hh_ctx_t* ctx;

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
    hh_ctx_t* ctx;

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
    int64_t a;
    int64_t b;
    hh_ctx_t* ctx;

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
        return enif_make_atom(
            env,
            hdr_values_are_equivalent(ctx->data, a, b) ? "true" : "false"
        );
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_lowest_at(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int64_t value;
    hh_ctx_t* ctx;

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
    int64_t value;
    hh_ctx_t* ctx;

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
    hh_ctx_t* ctx;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        hdr_percentiles_print(ctx->data, stdout, 5, 1.0, CLASSIC);
        return enif_make_atom(env, "ok");
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_print_csv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL)
    {
        hdr_percentiles_print(ctx->data, stdout, 5, 1.0, CSV);
        return enif_make_atom(env, "ok");
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_log_classic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char fname[64];
    hh_ctx_t* ctx;

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
        return enif_make_atom(env, "ok");
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}


ERL_NIF_TERM _hh_log_csv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char fname[64];
    hh_ctx_t* ctx;

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
        return enif_make_atom(env, "ok");
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_reset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx;

    ErlNifResourceType* ctx_type = (ErlNifResourceType*)enif_priv_data(env);
    if (ctx_type != NULL &&
        !enif_get_resource(env, argv[0], ctx_type, (void **)&ctx))
    {
        return enif_make_badarg(env);
    }

    if (ctx != NULL && ctx->data != NULL)
    {
        hdr_reset(ctx->data);
        return enif_make_atom(env, "ok");
    }

    return make_error(env, "bad_hdr_histogram_nif_impl");
}

ERL_NIF_TERM _hh_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    hh_ctx_t* ctx;

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

    return enif_make_atom(env, "ok");
}

static void init(ErlNifEnv* env)
{
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = (hh_ctx_t*)malloc(sizeof(hh_ctx_t));
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
    {"close", 1, _hh_close}
};

ERL_NIF_INIT(hdr_histogram, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
