# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove broken file
    rm Lib/test/test_pep263.py

    # Delete generated files
    rm Modules/glmodule.c
    rm Include/Python-ast.h Python/Python-ast.c
    rm Lib/stringprep.py
    rm Misc/Vim/python.vim
    mv Lib/plat-generic .
    rm -r Lib/plat-*
    rm -r Modules/_ctypes/libffi
    mv plat-generic Lib/
    grep generated -r . -l | grep encodings | xargs rm

    # Regenerate unicode
    rm Modules/unicodedata_db.h Modules/unicodename_db.h Objects/unicodetype_db.h
    for f in UnicodeData CompositionExclusions EastAsianWidth; do
        mv "../${f}-3.2.0.txt" .
        mv "../${f}-4.1.0.txt" "${f}.txt"
    done
    python Tools/unicode/makeunicodedata.py

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
    LDFLAGS="-L${LIBDIR}" \
        ./configure \
        --build=i386-unknown-linux-musl \
        --host=i386-unknown-linux-musl \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --with-system-ffi \
        --enable-ipv6
}

src_compile() {
    # Temporarily break include cycle
    patch -Np1 -i graminit-regen.patch
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

    # Undo change
    patch -Np1 -R -i graminit-regen.patch
    # Now build the main program
    make "${MAKEJOBS}" CFLAGS="-U__DATE__ -U__TIME__"
}

src_install() {
    default

    # Remove non-reproducible .pyc/o files
    find "${DESTDIR}" -name "*.pyc" -delete
    find "${DESTDIR}" -name "*.pyo" -delete
}
