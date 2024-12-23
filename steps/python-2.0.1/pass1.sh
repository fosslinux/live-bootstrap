# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Delete generated files
    rm Modules/glmodule.c
    rm Modules/unicodedata_db.h Objects/unicodetype_db.h
    rm Modules/sre_constants.h
    mv Lib/plat-generic .
    rm -r Lib/plat-*
    mv plat-generic Lib/
    grep generated -r . -l | grep encodings | xargs rm

    # Disable sre and unicodedata modules
    sed -i "/^_sre/d" Modules/Setup.in
    sed -i "/^unicodedata/d" Modules/Setup.in

    # Patch
    patch -Np1 -i disable-unicode.patch

    # Regenerate autoconf
    autoreconf-2.71 -fi
}

src_configure() {
    MACHDEP=linux ac_sys_system=Linux \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --with-wctype-functions
}

src_compile() {
    # Build pgen
    pushd Parser
    make -j1 pgen
    popd
    # Regen graminit.c and graminit.h
    pushd Grammar
    make -j1 graminit.c
    popd

    # Regenerate some Python scripts using the other regenerated files 
    gcc -o keyword keyword.c
    gcc -o token token.c
    # This gets all of the grammar tokens
    grep -E '\{1, "[^"]+"' Python/graminit.c | ./keyword > Lib/keyword.py.new
    mv Lib/keyword.py.new Lib/keyword.py
    ./token Lib/symbol.py < Include/graminit.h > Lib/symbol.py.new
    mv Lib/symbol.py.new Lib/symbol.py
    # These get all of the #defines that have to be translated
    grep '#define[[:space:]][A-Z]*[[:space:]][[:space:]]*[0-9][0-9]*' Include/token.h | ./token Lib/token.py > Lib/token.py.new
    mv Lib/token.py.new Lib/token.py

    # Now build the main program
    make -j1
}

src_install() {
    mkdir -p "${DESTDIR}/usr"
    default

    # Remove non-reproducible .pyc/o files
    find "${DESTDIR}" -name "*.pyc" -delete
    find "${DESTDIR}" -name "*.pyo" -delete
}
