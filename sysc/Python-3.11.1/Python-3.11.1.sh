# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Delete generated files that won't be regenerated
    rm Lib/pydoc_data/topics.py \
        Misc/stable_abi.toml

    # Regenerate ssl_data for ssl module
    rm Modules/_ssl_data_300.h Modules/_ssl_data.h
    python Tools/ssl/make_ssl_data.py ../openssl-1.1.1l Modules/_ssl_data_111.h

    # Regenerate encodings
    grep generated -r . -l | grep encodings | xargs rm
    mkdir Tools/unicode/in Tools/unicode/out
    mv ../CP437.TXT Tools/unicode/in/
    pushd Tools/unicode
    python gencodec.py in/ ../../Lib/encodings/
    popd

    # Regenerate stringprep
    rm Lib/stringprep.py
    mv ../rfc3454.txt .
    python Tools/unicode/mkstringprep.py > Lib/stringprep.py

    # Regenerate unicode
    rm Modules/unicodedata_db.h Modules/unicodename_db.h Objects/unicodetype_db.h
    mkdir -p Tools/unicode/data
    mv ../*.txt ../*.zip Tools/unicode/data/
    python Tools/unicode/makeunicodedata.py

    # Regenerate Lib/re/_casefix.py
    rm Lib/re/_casefix.py
    python Tools/scripts/generate_re_casefix.py Lib/re/_casefix.py

    # Regenerate Programs/test_frozenmain.h
    rm Programs/test_frozenmain.h
    python Programs/freeze_test_frozenmain.py Programs/test_frozenmain.h

    # Create dummy Python/stdlib_module_names.h
    echo 'static const char* _Py_stdlib_module_names[] = {};' > Python/stdlib_module_names.h

    # Regenerate autoconf
    autoreconf-2.71 -fi
}

src_configure() {
    CPPFLAGS="-U__DATE__ -U__TIME__" \
    LDFLAGS="-L/usr/lib/musl" \
        ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --with-system-ffi
}

src_compile() {
    # Regenerations
    # We have to choose the order ourselves because the Makefile is extremely lax about the order
    # First of all, do everything that doesn't use any C
    rm Modules/_blake2/blake2s_impl.c
    make regen-opcode \
        regen-opcode-targets \
        regen-typeslots \
        regen-token \
        regen-ast \
        regen-keyword \
        regen-sre \
        clinic \
        regen-pegen-metaparser \
        regen-pegen \
        regen-global-objects

    # Do the freeze regen process
    make regen-frozen
    make regen-deepfreeze
    make regen-global-objects

    make CPPFLAGS="-U__DATE__ -U__TIME__"

    # Regen Python/stdlib_module_names.h (you must have an existing build first)
    make regen-stdlib-module-names

    # Now rebuild with proper stdlib_module_names.h
    make CPPFLAGS="-U__DATE__ -U__TIME__"
}

src_install() {
    default
    ln -s "${PREFIX}/lib/musl/python3.11/lib-dynload" "${DESTDIR}${PREFIX}/lib/python3.11/lib-dynload"
    ln -s "${PREFIX}/bin/python3.11" "${DESTDIR}${PREFIX}/bin/python"
}
