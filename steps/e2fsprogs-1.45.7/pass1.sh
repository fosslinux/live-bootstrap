# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Get UTF txt files
    cp ../*.txt .

    # Rebuild libtool files
    rm config/config.guess config/config.sub config/ltmain.sh
    libtoolize -i

    AUTOMAKE=automake-1.10 ACLOCAL=aclocal-1.10 AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 autoreconf-2.64 -fi

    # Remove bison parser generated
    rm intl/plural.y

    # Setup for regeneratation of lib/ext2fs/utf8data.h
    rm lib/ext2fs/utf8data.h

    # Fix compile_et
    sed -r -i "s/ > ?outfile//" lib/et/et_c.awk lib/et/et_h.awk lib/ss/ct_c.awk

    # Disable int
    sed -i "s/@LIBINTL@//" MCONFIG.in
}

src_configure() {
    # defrag fails to build with musl 1.2.4
    ./configure --prefix="${PREFIX}" \
        --sbindir="${PREFIX}/bin" \
        --disable-tls \
        --disable-defrag \
        with_udev_rules_dir=no \
        with_systemd_unit_dir=no
}

src_compile() {
    # Regen utf8data
    make -C util mkutf8data
    util/mkutf8data -o lib/ext2fs/utf8data.h
    # Why does mkutf8data generate something not usable by build?
    sed -i "s/nfkdi/nfdi/g" lib/ext2fs/utf8data.h

    default
}
