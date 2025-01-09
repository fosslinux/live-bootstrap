# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove broken file
    rm Lib/test/test_pep263.py

    # Delete generated files
    rm Modules/glmodule.c
    rm Modules/unicodedata_db.h Objects/unicodetype_db.h
    rm Lib/stringprep.py
    mv Lib/plat-generic .
    rm -r Lib/plat-*
    mv plat-generic Lib/
    grep generated -r . -l | grep encodings | xargs rm

    # Disable unicode
    patch -Np1 -i disable-unicode.patch

    # Regenerate sre_constants.h
    rm Modules/sre_constants.h
    python Lib/sre_constants.py

    # Regen ast module
    rm Lib/compiler/ast.py
    pushd Tools/compiler
    python astgen.py > ../../Lib/compiler/ast.py
    popd

    # Regenerate autoconf
    autoreconf-2.71 -fi
}

src_configure() {
    MACHDEP=linux ac_sys_system=Linux \
    CFLAGS="-U__DATE__ -U__TIME__" \
        ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --with-wctype-functions \
        --enable-ipv6
}

src_compile() {
    # Build pgen
    make "${MAKEJOBS}" Parser/pgen
    # Regen graminit.c and graminit.h
    make "${MAKEJOBS}" Include/graminit.h

    # Regenerate some Python scripts using the other regenerated files
    # Must move them out to avoid using Lib/ module files which are
    # incompatible with running version of Python
    cp Lib/{symbol,keyword,token}.py .
    python symbol.py
    python keyword.py
    python token.py

    # Now build the main program
    make "${MAKEJOBS}" CFLAGS="-U__DATE__ -U__TIME__"
}

src_install() {
    default

    # Remove non-reproducible .pyc/o files
    find "${DESTDIR}" -name "*.pyc" -delete
    find "${DESTDIR}" -name "*.pyo" -delete
}
