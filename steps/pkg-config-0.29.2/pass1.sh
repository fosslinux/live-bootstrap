# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm glib/glib/gunidecomp.h glib/glib/gunibreak.h glib/glib/gscripttable.h \
        glib/glib/gunichartables.h

    pushd glib/glib
    mkdir unidata
    mv ../../../*.txt unidata/
    perl gen-unicode-tables.pl -both 6.2.0 unidata
    perl gen-script-table.pl unidata/Scripts-6.2.0.txt > gscripttable.h
    popd

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    # We use internal glib because standalone glib library depends on
    # pkg-config and other software (python/meson) that we do not have.
    ./configure \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl \
        --with-internal-glib
}
