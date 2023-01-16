# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Delete generated files
    rm Include/Python-ast.h Python/Python-ast.c
    rm Lib/stringprep.py
    rm Lib/pydoc_data/topics.py
    rm Misc/Vim/python.vim
    rm -r Modules/_ctypes/libffi
    mv Lib/plat-generic .
    rm -r Lib/plat-*
    mv plat-generic Lib/
    grep generated -r . -l | grep encodings | xargs rm

    # Regenerate encodings
    mkdir Tools/unicode/in Tools/unicode/out
    mv ../CP437.TXT Tools/unicode/in/
    pushd Tools/unicode
    python gencodec.py in/ ../../Lib/encodings/
    popd

    # Regenerate unicode
    rm Modules/unicodedata_db.h Modules/unicodename_db.h Objects/unicodetype_db.h
    for f in UnicodeData CompositionExclusions EastAsianWidth DerivedCoreProperties DerivedNormalizationProps; do
        mv "../${f}-3.2.0.txt" .
        mv "../${f}-5.1.0.txt" "${f}.txt"
    done
    python Tools/unicode/makeunicodedata.py

    # Regenerate sre_constants.h
    rm Modules/sre_constants.h
    python2.5 Lib/sre_constants.py

    # Regenerate autoconf
    autoreconf-2.71 -fi
}

src_configure() {
    MACHDEP=linux ac_sys_system=Linux \
    CFLAGS="-U__DATE__ -U__TIME__" \
    LDFLAGS="-L/usr/lib/musl" \
        ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --build=i386-unknown-linux-musl \
        --host=i386-unknown-linux-musl \
        --with-pydebug \
        --with-system-ffi
}

src_compile() {
    # Temporarily break include cycle
    patch -Np0 -i graminit-regen.patch
    # Build pgen
    make Parser/pgen
    # Regen graminit.c and graminit.h
    make Include/graminit.h

    # Regenerate some Python scripts using the other regenerated files
    # Must move them out to avoid using Lib/ module files which are
    # incompatible with running version of Python
    cp Lib/{symbol,keyword,token}.py .
    python symbol.py
    python keyword.py
    python token.py

    # Undo change
    patch -Np0 -R -i graminit-regen.patch
    # Now build the main program
    make CFLAGS="-U__DATE__ -U__TIME__"
}

src_install() {
    default
    ln -s "${PREFIX}/lib/musl/python3.1/lib-dynload" "${DESTDIR}${PREFIX}/lib/python3.1/lib-dynload"
    ln -s "${PREFIX}/bin/python3.1" "${DESTDIR}${PREFIX}/bin/python"

    # Remove non-reproducible .pyc/o files
    find "${DESTDIR}" -name "*.pyc" -delete
    find "${DESTDIR}" -name "*.pyo" -delete

    # This file is not reproducible and I don't care to fix it
    rm "${DESTDIR}/${PREFIX}/lib/python3.1/lib2to3/"{Pattern,}"Grammar3.1.5.final.0.pickle"
}
