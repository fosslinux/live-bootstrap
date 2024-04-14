# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# XXX: If you change the version of this, you must update the corresponding
# tarball in Python 3.11.

src_prepare() {
    default

    # Remove a bunch of pregenerated files
    # thanks for making these easy to find :)
    find . -name build.info -exec grep 'GENERATE\[' {} \; | sed 's/.*\[//' | sed 's/\].*$//' | xargs -I{} find . -name {} -delete
}

src_configure() {
    ./config --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        no-shared linux-generic32
}

src_compile() {
    declare -x SOURCE_DATE_EPOCH=1638831119
    default
}

src_install() {
    default

    rm -r "${DESTDIR}${PREFIX}/share/doc/openssl/html/man"{1,3,5,7}
    rm -r "${DESTDIR}${PREFIX}/share/man/man"{1,3,5,7}
    rm -r "${DESTDIR}${PREFIX}/ssl/misc"
}
