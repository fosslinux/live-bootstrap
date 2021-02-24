# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_configure() {
    # TODO: use autoconf to regenerate configure

    AR="tcc -ar" RANLIB="true" CC="tcc -D __GLIBC_MINOR__=6" \
        ./configure \
        --disable-nls \
        --disable-shared \
        --disable-werror \
        --build=i386-unknown-linux \
        --host=i386-unknown-linux \
        --target=i386-unknown-linux \
        --with-sysroot=/after \
        --disable-64-bit-bfd

    # TODO: Find a way to avoid these hacks
    sed -i '/#undef pid_t/d' libiberty/config.in
    sed -i '/#undef uintptr_t/d' libiberty/config.in
    sed -i 's/C_alloca/alloca/g' libiberty/alloca.c
    sed -i 's/C_alloca/alloca/g' include/libiberty.h
}
