/*
 SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com>
 SPDX-License-Identifier: MIT
 */
#define CONFIG_USE_DEFAULT_CONFIG 0
#define CONFIG_HAVE_OWN_OFLAGS 0
#define CONFIG_HAVE_OWN_ERRNO 1
#define CONFIG_HAVE_OWN_ASSERT 0
#define CONFIG_BLOCK_DEV_CACHE_SIZE 16
typedef long long unsigned int uint64_t;
typedef long long int int64_t;
#define fseeko fseek
#define ftello ftell
#define PRIu32 "u"
#define PRId32 "d"
#define PRIx32 "x"
#define PRIu16 "hu"

