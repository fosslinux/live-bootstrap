# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove pregenerated table in strtofr.c
    sed -i '/^  {/,/ };$/d' src/strtofr.c
    cp src/strtofr.c{,.old}
    sed -i '/int RedInvLog2Table/ s/$/};/' src/strtofr.c

    rm doc/*.info

    # testfiles
    rm tests/tfpif_*.dat tests/tstrtofr.c

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --disable-shared

    # Disable tuning as that might cause non-reproducible build
    mv mparam.h src
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true DESTDIR="${DESTDIR}"

    pushd src
    cat > strtofr_gen.c <<EOF
#include <stdio.h>
#include <gmp.h>
#include <mpfr.h>
EOF
    # Enable the bit of code that generates the table
    sed -n '/^#define N 8$/,/^}$/p' strtofr.c >> strtofr_gen.c
    gcc strtofr_gen.c -o strtofr_gen -std=gnu99 -I. -L.libs -lmpfr -lgmp
    # ordering of 2>&1 >/dev/null is intentional here;
    # stdout -> null
    # stderr -> file (NOT null)
    ./strtofr_gen 2>strtofr_table >/dev/null
    echo "};" >> strtofr_table
    sed "/int RedInvLog2Table/ r strtofr_table" strtofr.c.old > strtofr.c
    popd

    make "${MAKEJOBS}" MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}

src_postprocess() {
    # For some unexplainable reason, stripping mpfr breaks GCC 10 build.
    # I cannot make any sense of the error, so skip for now.
    :
}
