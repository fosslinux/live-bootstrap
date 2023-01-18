# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# XXX: If you change the version of this, you must update the corresponding
# tarball in Python 3.11.

src_prepare() {
    default

    # Remove two useless manpages that code in hostname
    rm doc/man5/x509v3_config.pod doc/man5/config.pod

    # Remove a bunch of pregenerated files
    # thanks for making these easy to find :)
    find . -name build.info -exec grep 'GENERATE\[' {} \; | sed 's/.*\[//' | sed 's/\].*$//' | xargs -I{} find . -name {} -delete
}

src_configure() {
    MACHINE=i386 ./config --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        no-shared
}

src_compile() {
    export SOURCE_DATE_EPOCH=1638831119
    default
}

src_install() {
    default

    rm -r "${DESTDIR}${PREFIX}/share/doc/openssl/html/man"{1,3,7}
    rm -r "${DESTDIR}${PREFIX}/share/man/man"{1,3,7}
    rm -r "${DESTDIR}${PREFIX}/ssl/misc"
}
