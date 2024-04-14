# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Delete generated files that won't be regenerated
    rm Lib/pydoc_data/topics.py \
        Misc/stable_abi.toml

    # Regenerate ssl_data for ssl module
    rm Modules/_ssl_data_111.h Modules/_ssl_data.h
    python -B Tools/ssl/make_ssl_data.py ../openssl-3.0.13 Modules/_ssl_data_300.h
    sed -i 's#$(srcdir)/Modules/_ssl_data.h ##' Makefile.pre.in
    sed -i 's#$(srcdir)/Modules/_ssl_data_111.h ##' Makefile.pre.in

    # Regenerate encodings
    grep generated -r . -l | grep encodings | xargs rm
    mkdir Tools/unicode/in Tools/unicode/out
    mv ../CP437.TXT Tools/unicode/in/
    pushd Tools/unicode
    python -B gencodec.py in/ ../../Lib/encodings/
    popd

    # Regenerate stringprep
    rm Lib/stringprep.py
    mv ../rfc3454.txt .
    python -B Tools/unicode/mkstringprep.py > Lib/stringprep.py

    # Regenerate unicode
    rm Modules/unicodedata_db.h Modules/unicodename_db.h Objects/unicodetype_db.h
    mkdir -p Tools/unicode/data
    mv ../*.txt ../*.zip Tools/unicode/data/
    python -B Tools/unicode/makeunicodedata.py

    # Regenerate Lib/re/_casefix.py
    rm Lib/re/_casefix.py
    python -B Tools/scripts/generate_re_casefix.py Lib/re/_casefix.py

    # Regenerate Programs/test_frozenmain.h
    rm Programs/test_frozenmain.h
    python -B Programs/freeze_test_frozenmain.py Programs/test_frozenmain.h

    # Create dummy Python/stdlib_module_names.h
    echo 'static const char* _Py_stdlib_module_names[] = {};' > Python/stdlib_module_names.h

    # Regenerate autoconf
    autoreconf-2.71 -fi
}

src_configure() {
    mv Setup.local Modules
    MACHDEP=linux ac_sys_system=Linux \
    CPPFLAGS="-U__DATE__ -U__TIME__" \
    PKG_CONFIG_PATH="${LIBDIR}/pkgconfig/" \
    LDFLAGS="-static" \
        ./configure \
        --build=i386-unknown-linux-musl \
        --host=i386-unknown-linux-musl \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --with-system-ffi \
        --disable-shared
}

src_compile() {
    # Regenerations
    # We have to choose the order ourselves because the Makefile is extremely lax about the order
    # First of all, do everything that doesn't use any C
    rm Modules/_blake2/blake2s_impl.c
    PYTHONDONTWRITEBYTECODE=1 \
        make "${MAKEJOBS}" \
        regen-opcode \
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
    PYTHONDONTWRITEBYTECODE=1 make "${MAKEJOBS}" regen-frozen
    PYTHONDONTWRITEBYTECODE=1 make "${MAKEJOBS}" regen-deepfreeze
    PYTHONDONTWRITEBYTECODE=1 make "${MAKEJOBS}" regen-global-objects

    make "${MAKEJOBS}" CPPFLAGS="-U__DATE__ -U__TIME__"

    # Regen Python/stdlib_module_names.h (you must have an existing build first)
    PYTHONDONTWRITEBYTECODE=1 make "${MAKEJOBS}" regen-stdlib-module-names

    # Now rebuild with proper stdlib_module_names.h
    PYTHONDONTWRITEBYTECODE=1 make "${MAKEJOBS}" CPPFLAGS="-U__DATE__ -U__TIME__"
}

src_install() {
    default
    ln --symbolic --relative "${DESTDIR}${LIBDIR}/python3.11/lib-dynload" "${DESTDIR}${PREFIX}/lib/python3.11/lib-dynload"
    ln --symbolic --relative "${DESTDIR}${PREFIX}/bin/python3.11" "${DESTDIR}${PREFIX}/bin/python"
}
