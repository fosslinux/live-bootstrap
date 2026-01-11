# SPDX-FileCopyrightText: 2021-22 Samuel Tyler <samuel@samuelt.me>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    find . \( -name '*.info*' \
        -o -name '*.gmo' \
        -o -name '*.mo' \) -delete
    find . \( -name '*.html*' -o -name '*.1*' \) \
        -not -path "./gettext-runtime/intl-csharp/doc/*" \
        -not -path "./gettext-tools/doc/tutorial.html" \
        -not -path "./gettext-tools/doc/FAQ.html" \
        -delete \
        -exec touch {} +

    # bison
    rm gettext-runtime/intl/plural.{c,h} \
        gettext-tools/src/{read-po-gram,cldr-plural}.{c,h}

    # Misc files
    find . -name "*.class" -delete -exec touch {} +
    touch gettext-tools/gnulib-lib/javaversion.class
    rm gettext-tools/m4/csharpexec-test.exe
    touch gettext-tools/m4/csharpexec-test.exe
    rm -r gettext-tools/tests/testdata/repo.tar.gz \
        gettext-tools/tests/testdata/dprog.*.d \
        gettext-tools/tests/mm-viet.comp.po \
        gettext-tools/tests/qttest*.qm \
        libtextstyle/{,gnulib-local}/tests/test-term-ostream-xterm-xf86-v32.out
    touch libtextstyle/{,gnulib-local}/tests/test-term-ostream-xterm-xf86-v32.out

    # gnulib-local/lib/libxml
    local libxml2
    libxml2=$(realpath ../libxml2-v2.9.9)
    mv post-global-regen.patch "$libxml2"
    pushd "$libxml2"
    for i in include/libxml/globals.h globals.c; do
        sed "/Everything starting from the line below/q" $i > $i.top
        mv $i.top $i
    done
    rm include/libxml/xmlunicode.h xmlunicode.c \
        include/libxml/chvalid.h chvalid.c
    mv ../*-4.0.1.txt .
    python2.3 genUnicode.py
    python2.3 genChRanges.py
    python2.3 build_glob.py
    patch -Np1 -i post-global-regen.patch
    popd

    pushd gnulib-local/lib/libxml
    for f in xmlunicode chvalid globals; do
        cp "$libxml2/$f.c" "$f.c"
        cp "$libxml2/include/libxml/$f.h" "$f.in.h"
    done
    popd

    # Regenerate gnulib files
    rm {gettext-runtime,gettext-runtime/intl,gettext-runtime/libasprintf,gettext-tools}/gnulib-lib/uniwidth/width*.h \
        {gettext-runtime,gettext-runtime/intl,gettext-runtime/libasprintf,gettext-tools}/gnulib-lib/unictype/ctype*.h \
        {gettext-runtime,gettext-runtime/intl,gettext-runtime/libasprintf,gettext-tools}/gnulib-lib/unicase/tolower.h \
        gettext-tools/libgettextpo/uniwidth/width*.h \
        gettext-tools/libgettextpo/unictype/ctype*.h \
        gettext-tools/libgettextpo/unicase/tolower.h \
        gettext-tools/gnulib-lib/unicase/{tocasefold,ignorable,cased}.h \
        gettext-tools/gnulib-lib/unicase/special-casing-table.* \
        gettext-tools/gnulib-lib/unictype/combiningclass.h \
        gettext-tools/{gnulib-lib,libgettextpo}/unictype/pr_*.h \
        gettext-tools/{gnulib-lib,libgettextpo}/unilbrk/{lbrktables.c,lbrkprop*.h} \
        gettext-tools/gnulib-lib/uniname/{uninames.h,gen-uninames.lisp} \
        gettext-tools/gnulib-lib/uninorm/{de,}composition-table* \
        gettext-tools/gnulib-tests/unicase/test-{uc_tolower,ignorable,cased}.c \
        gettext-tools/gnulib-tests/unictype/test-{ctype,pr,sy}_*.c
    rm libtextstyle/lib/uniwidth/width*.h \
        libtextstyle/lib/unictype/ctype_*.h \
        libtextstyle/lib/unicase/tolower.h \
        libtextstyle/lib/*-ostream.{c,h} \
        libtextstyle/lib/*_ostream.* \
        libtextstyle/lib/ostream.{c,h,vt.h,priv.h}
    rm -r gettext-tools/gnulib-lib/libxml libtextstyle/lib/libxml

    pushd ../gnulib-b5eb878
    rm lib/unictype/ctype*.h \
        lib/unicase/tolower.h \
        lib/uniwidth/width*.h

    pushd lib
    gcc -Iunictype -o gen-uni-tables gen-uni-tables.c
    mv ../../*-16.0.0.txt ../../PropList-3.0.1.txt .
    ./gen-uni-tables UnicodeData-16.0.0.txt \
        PropList-16.0.0.txt \
        DerivedCoreProperties-16.0.0.txt \
        emoji-data-16.0.0.txt \
        ArabicShaping-16.0.0.txt \
        Scripts-16.0.0.txt \
        Blocks-16.0.0.txt \
        PropList-3.0.1.txt \
        BidiMirroring-16.0.0.txt \
        EastAsianWidth-16.0.0.txt \
        LineBreak-16.0.0.txt \
        WordBreakProperty-16.0.0.txt \
        GraphemeBreakProperty-16.0.0.txt \
        CompositionExclusions-16.0.0.txt \
        SpecialCasing-16.0.0.txt \
        CaseFolding-16.0.0.txt \
        16.0.0
    popd
    popd

    # gnulib + autotools
    GNULIB_SRCDIR=$(realpath ../gnulib-b5eb878) ./autogen.sh

    # archive.dir.tar
    local archive temp
    archive="$(realpath gettext-tools/misc/archive.dir.tar)"
    temp="$(mktemp -d)"
    pushd "$temp"
    tar -xf "$archive"
    sed -i.bak "s/%expect 10/%expect 7/" gettext-0.10.*/intl/plural.y
    for f in gettext-0.10.*/intl/plural.y; do
        touch -r "$f.bak" "$f"
        touch -r "$f.bak" "$(dirname "$f")"
    done
    find . -path "*/intl/plural.c" | while read -r file; do
        pushd "$(dirname "$file")"
        rm plural.c
        bison -o plural.c plural.y
        touch -r plural.y plural.c
        touch -r plural.y .
        popd
    done
    tar -cf "$archive" gettext-*
    popd
    rm -rf "$temp"
}

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --enable-static \
        --disable-shared \
        --disable-java
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
