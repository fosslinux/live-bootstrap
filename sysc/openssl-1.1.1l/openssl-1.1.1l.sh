# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

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
