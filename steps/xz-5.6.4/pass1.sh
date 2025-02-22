# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# sources note: Unfortunately, xz's xz tarballs use SHA-256 checksum, which
# is not widely supported (including by xz), so we use bz2 tarball instead.

src_prepare() {
    # Delete translation catalogs
    rm po/*.gmo

    # Delete generated documentation
    rm -rf po4a/man

    # Would have detected the xz backdoor
    rm tests/files/*.{x,l}z

    # Regenerate these c files/headers
    rm src/liblzma/rangecoder/price_table.c src/liblzma/lzma/fastpos_table.c \
        src/liblzma/lz/lz_encoder_hash_table.h \
        src/liblzma/check/crc{32,64}_table_*.h

    pushd src/liblzma/rangecoder
    gcc -std=c99 -o price_tablegen price_tablegen.c
    ./price_tablegen > price_table.c
    popd

    pushd src/liblzma/lzma
    gcc -std=c99 -o fastpos_tablegen fastpos_tablegen.c
    ./fastpos_tablegen > fastpos_table.c
    popd

    pushd src/liblzma/check
    gcc -std=c99 -o crc32_tablegen_le crc32_tablegen.c
    ./crc32_tablegen_le > crc32_table_le.h
    gcc -std=c99 -DWORDS_BIGENDIAN -o crc32_tablegen_be crc32_tablegen.c
    ./crc32_tablegen_be > crc32_table_be.h
    gcc -std=c99 -DLZ_HASH_TABLE -o crc32_tablegen_hashtable crc32_tablegen.c
    ./crc32_tablegen_hashtable > ../lz/lz_encoder_hash_table.h

    gcc -std=c99 -o crc64_tablegen_le crc64_tablegen.c
    ./crc64_tablegen_le > crc64_table_le.h
    gcc -std=c99 -DWORDS_BIGENDIAN -o crc64_tablegen_be crc64_tablegen.c
    ./crc64_tablegen_be > crc64_table_be.h
    popd

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 AUTOCONF=autoconf-2.69 AUTOM4TE=autom4te-2.69 autoreconf-2.69 -f
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --disable-shared \
        --disable-nls \
        --build=i386-unknown-linux-musl \
        --libdir="${LIBDIR}"
}
