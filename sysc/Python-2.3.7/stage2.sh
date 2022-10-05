# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove broken file
    rm Lib/test/test_pep263.py

    # Delete generated files
    rm Modules/glmodule.c
    rm Lib/stringprep.py
    mv Lib/plat-generic .
    rm -r Lib/plat-*
    mv plat-generic Lib/
    grep generated -r . -l | grep encodings | xargs rm

    # Regenerate unicode 
    rm Modules/unicodedata_db.h Modules/unicodename_db.h Objects/unicodetype_db.h
    mv ../UnicodeData-3.2.0.txt UnicodeData.txt
    mv ../CompositionExclusions-3.2.0.txt CompositionExclusions.txt
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
    CFLAGS="-U__DATE__ -U__TIME__" \
        ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl"
}

src_compile() {
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

    # Now build the main program
    make CFLAGS="-U__DATE__ -U__TIME__"
}

src_install() {
    default

    # Remove non-reproducible .pyc/o files
    find "${DESTDIR}" -name "*.pyc" -delete
    find "${DESTDIR}" -name "*.pyo" -delete
}
