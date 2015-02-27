/**
 * hdr_histogram.c
 * Written by Michael Barker and released to the public domain,
 * as explained at http://creativecommons.org/publicdomain/zero/1.0/
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <zlib.h>
#include <errno.h>
#include <sys/stat.h>
#include <ctype.h>
#include <math.h>
#include <time.h>

#include "hdr_histogram.h"
#include "hdr_histogram_bin.h"
#include "hdr_histogram_log.h"

#ifdef __APPLE__

#include <libkern/OSByteOrder.h>

#define htobe16(x) OSSwapHostToBigInt16(x)
#define htole16(x) OSSwapHostToLittleInt16(x)
#define be16toh(x) OSSwapBigToHostInt16(x)
#define le16toh(x) OSSwapLittleToHostInt16(x)

#define htobe32(x) OSSwapHostToBigInt32(x)
#define htole32(x) OSSwapHostToLittleInt32(x)
#define be32toh(x) OSSwapBigToHostInt32(x)
#define le32toh(x) OSSwapLittleToHostInt32(x)

#define htobe64(x) OSSwapHostToBigInt64(x)
#define htole64(x) OSSwapHostToLittleInt64(x)
#define be64toh(x) OSSwapBigToHostInt64(x)
#define le64toh(x) OSSwapLittleToHostInt64(x)

#elif __linux__

#include <endian.h>

#else

#warning "Platform not supported\n"

#endif

#define FAIL_AND_CLEANUP(label, error_name, error) \
    do                      \
    {                       \
        error_name = error; \
        goto label;         \
    }                       \
    while (0)

enum zero_strategy { ZERO_ALL, ZERO_NONE };


// ######## ##    ##  ######   #######  ########  #### ##    ##  ######
// ##       ###   ## ##    ## ##     ## ##     ##  ##  ###   ## ##    ##
// ##       ####  ## ##       ##     ## ##     ##  ##  ####  ## ##
// ######   ## ## ## ##       ##     ## ##     ##  ##  ## ## ## ##   ####
// ##       ##  #### ##       ##     ## ##     ##  ##  ##  #### ##    ##
// ##       ##   ### ##    ## ##     ## ##     ##  ##  ##   ### ##    ##
// ######## ##    ##  ######   #######  ########  #### ##    ##  ######

static const int32_t ENCODING_COOKIE      = 0x1c849308 + (8 << 4);
static const int32_t COMPRESSION_COOKIE   = 0x1c849309 + (8 << 4);
static const int32_t NOCOMPRESSION_COOKIE = 0x1c84930A + (8 << 4);

typedef struct __attribute__((__packed__))
{
    int32_t cookie;
    int32_t significant_figures;
    int64_t lowest_trackable_value;
    int64_t highest_trackable_value;
    int64_t total_count;
    int64_t counts[0];
} _encoding_flyweight;

typedef struct __attribute__((__packed__))
{
    int32_t cookie;
    int32_t length;
    uint8_t data[0];
} _compression_flyweight;

int hdr_encode_uncompressed(
    struct hdr_histogram* h,
    uint8_t** compressed_histogram,
    int* compressed_len)
{
  //const int counts_per_chunk = 512;
  //int64_t chunk[counts_per_chunk];

    uint8_t* buf = NULL;
    int idx = sizeof(_compression_flyweight);
    int len = 4096;

    int result = 0;

    if ((buf = (uint8_t*) malloc(len * sizeof(uint8_t))) == NULL)
    {
        FAIL_AND_CLEANUP(cleanup, result, ENOMEM);
    }

    _compression_flyweight* comp_fw = (_compression_flyweight*) buf;
    _encoding_flyweight encode_fw;

    encode_fw.cookie                  = htobe32(ENCODING_COOKIE);
    encode_fw.significant_figures     = htobe32(h->significant_figures);
    encode_fw.lowest_trackable_value  = htobe64(h->lowest_trackable_value);
    encode_fw.highest_trackable_value = htobe64(h->highest_trackable_value);
    encode_fw.total_count             = htobe64(h->total_count);

    //int counts_index = 0;

    memcpy(buf+idx, &encode_fw, sizeof(_encoding_flyweight));
    idx += sizeof(_encoding_flyweight);


    for (int i = 0; i < h->counts_len; i++)
    {

      if (idx + sizeof(uint64_t) > len) {
        int new_len = len * 2;
        uint8_t* new_buf = (uint8_t*) realloc(buf, new_len * sizeof(uint8_t));
        if (NULL == new_buf)
          {
            FAIL_AND_CLEANUP(cleanup, result, ENOMEM);
          }
        buf = new_buf;
        len = new_len;

      }
      buf[idx] = htobe64(h->counts[i]);
      idx += sizeof(uint64_t);
    }
    uint8_t* new_buf = (uint8_t*) malloc(idx * sizeof(uint8_t));
    if (NULL == new_buf)
      {
        FAIL_AND_CLEANUP(cleanup, result, ENOMEM);
      }
    memcpy(new_buf, buf, idx);
    free(buf);
    comp_fw = (_compression_flyweight*) new_buf;
    comp_fw->cookie = htobe32(NOCOMPRESSION_COOKIE);
    comp_fw->length = htobe32(idx);
    *compressed_histogram = new_buf;
    *compressed_len = idx;

cleanup:
    if (result != 0)
    {
        free(buf);
    }

    return result;
}

// ########  ########  ######   #######  ########  #### ##    ##  ######
// ##     ## ##       ##    ## ##     ## ##     ##  ##  ###   ## ##    ##
// ##     ## ##       ##       ##     ## ##     ##  ##  ####  ## ##
// ##     ## ######   ##       ##     ## ##     ##  ##  ## ## ## ##   ####
// ##     ## ##       ##       ##     ## ##     ##  ##  ##  #### ##    ##
// ##     ## ##       ##    ## ##     ## ##     ##  ##  ##   ### ##    ##
// ########  ########  ######   #######  ########  #### ##    ##  ######

int hdr_decode(
    uint8_t* buffer, size_t length, struct hdr_histogram** histogram)
{
    int result = 0;

    if (length < sizeof(_compression_flyweight))
    {
        FAIL_AND_CLEANUP(cleanup, result, EINVAL);
    }

    _compression_flyweight* compression_flyweight = (_compression_flyweight*) buffer;
    switch be32toh(compression_flyweight->cookie) {
      case NOCOMPRESSION_COOKIE:
        result = hdr_decode_uncompressed(buffer, length, histogram);
        break;
      case COMPRESSION_COOKIE:
        result = hdr_decode_compressed(buffer, length, histogram);
        break;
      default:
        FAIL_AND_CLEANUP(cleanup, result, HDR_COMPRESSION_COOKIE_MISMATCH);
      }

 cleanup:

    return result;

}

int hdr_decode_uncompressed(
    uint8_t* buffer, size_t length, struct hdr_histogram** histogram)
{
    int64_t* counts_array;
    struct hdr_histogram* h = NULL;
    int result = 0;

    int64_t counts_tally = 0;

    if (length < sizeof(_compression_flyweight))
    {
        FAIL_AND_CLEANUP(cleanup, result, EINVAL);
    }


    _compression_flyweight* compression_flyweight = (_compression_flyweight*) buffer;
    _encoding_flyweight encoding_flyweight;

    if (NOCOMPRESSION_COOKIE != be32toh(compression_flyweight->cookie))
    {
        FAIL_AND_CLEANUP(cleanup, result, HDR_COMPRESSION_COOKIE_MISMATCH);
    }

    memcpy((void*) &encoding_flyweight, compression_flyweight->data, sizeof(_encoding_flyweight));
    counts_array = (int64_t *) (compression_flyweight->data + sizeof(_encoding_flyweight));

    if (ENCODING_COOKIE != be32toh(encoding_flyweight.cookie))
    {
        FAIL_AND_CLEANUP(cleanup, result, HDR_ENCODING_COOKIE_MISMATCH);
    }

    int64_t lowest_trackable_value = be64toh(encoding_flyweight.lowest_trackable_value);
    int64_t highest_trackable_value = be64toh(encoding_flyweight.highest_trackable_value);
    int32_t significant_figures = be32toh(encoding_flyweight.significant_figures);

    if (hdr_init(
        lowest_trackable_value,
        highest_trackable_value,
        significant_figures,
        &h) != 0)
    {
        FAIL_AND_CLEANUP(cleanup, result, ENOMEM);
    }

    h->total_count = be64toh(encoding_flyweight.total_count);

    int counts_index = 0;
    int available_counts = 0;
    for (int i = 0; i < available_counts && counts_index < h->counts_len; i++)
      {
        h->counts[counts_index++] = be64toh(counts_array[i]);
        counts_tally += h->counts[counts_index - 1];
      }
cleanup:

    if (result != 0)
    {
        free(h);
    }
    else if (NULL == *histogram)
    {
        *histogram = h;
    }
    else
    {
        hdr_add(*histogram, h);
        free(h);
    }

    return result;
}
