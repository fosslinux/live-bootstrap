# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=5a18dfbf9cd8451511ff33fdf763aec8fde2af6188fe0779f322974de04cc1c7

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
