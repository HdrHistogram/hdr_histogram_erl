// talen from https://www.initworks.com/wiki/display/public/Solaris+common+compiler+and+linker+errors
        #include <sys/isa_defs.h>
        # include <sys/byteorder.h>
        # define betoh16(x) BE_16(x)
        # define letoh16(x) LE_16(x)
        # define betoh32(x) BE_32(x)
        # define letoh32(x) LE_32(x)
        # define betoh64(x) BE_64(x)
        # define letoh64(x) LE_64(x)
        #define htobe16(x) BE_16(x)
        #define be16toh(x) BE_16(x)
        #define htobe32(x) BE_32(x)
        #define be32toh(x) BE_32(x)
        #define htobe64(x) BE_64(x)
        #define be64toh(x) BE_64(x)
// Solaris defines endian by setting _LITTLE_ENDIAN or _BIG_ENDIAN
# ifdef _BIG_ENDIAN
#  define IS_BIG_ENDIAN
# endif
# ifdef _LITTLE_ENDIAN
#  define IS_LITTLE_ENDIAN
# endif
                // Make sure we got some kind of endian (but not both)
#if defined(IS_BIG_ENDIAN) == defined(IS_LITTLE_ENDIAN)
# error "Failed to get endian type for this system"
#endif
 
                // Define bswap functions if we didn't get any from the system headers
#ifndef BSWAP_16
# define BSWAP_16(x) ( \
                    ((uint16_t)(x) & 0x00ffU) << 8 | \
                    ((uint16_t)(x) & 0xff00U) >> 8)
#endif
#ifndef BSWAP_32
# define BSWAP_32(x) ( \
                    ((uint32_t)(x) & 0x000000ffU) << 24 | \
                    ((uint32_t)(x) & 0x0000ff00U) << 8 | \
                    ((uint32_t)(x) & 0x00ff0000U) >> 8 | \
                    ((uint32_t)(x) & 0xff000000U) >> 24)
#endif
#ifndef BSWAP_64
# define BSWAP_64(x) ( \
                    ((uint64_t)(x) & 0x00000000000000ffULL) << 56 | \
                    ((uint64_t)(x) & 0x000000000000ff00ULL) << 40 | \
                    ((uint64_t)(x) & 0x0000000000ff0000ULL) << 24 | \
                    ((uint64_t)(x) & 0x00000000ff000000ULL) << 8 | \
                    ((uint64_t)(x) & 0x000000ff00000000ULL) >> 8 | \
                    ((uint64_t)(x) & 0x0000ff0000000000ULL) >> 24 | \
                    ((uint64_t)(x) & 0x00ff000000000000ULL) >> 40 | \
                    ((uint64_t)(x) & 0xff00000000000000ULL) >> 56)
#endif
 
                // Define conversion functions if we didn't get any from the system headers
#ifndef BE_16
                // Big endian system, swap when converting to/from little endian
# if defined IS_BIG_ENDIAN
#  define BE_16(x) (x)
#  define BE_32(x) (x)
#  define BE_64(x) (x)
#  define LE_16(x) BSWAP_16(x)
#  define LE_32(x) BSWAP_32(x)
#  define LE_64(x) BSWAP_64(x)
                // Little endian system, swap when converting to/from big endian
# elif defined IS_LITTLE_ENDIAN
#  define BE_16(x) BSWAP_16(x)
#  define BE_32(x) BSWAP_32(x)
#  define BE_64(x) BSWAP_64(x)
#  define LE_16(x) (x)
#  define LE_32(x) (x)
#  define LE_64(x) (x)
#endif
#endif
