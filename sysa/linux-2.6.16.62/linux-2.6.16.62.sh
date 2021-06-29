# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    mv config .config
    # generate include/linux/autoconf.h -- we do not have gperf rn to do it the normal way
    # Transform each of the CONFIG_* options that are =y into header
    grep -E '=y$' .config | sed 's/=y$/ 1/' | sed 's/^/#define /' >> include/linux/autoconf.h
    # Transform each of the CONFIG_* options that are unset into headers
    grep -E ' is not set$' .config | sed 's/ is not set$//' | sed 's/#/#undef/' >> include/linux/autoconf.h
    # Transform each of the non-boolean options into headers
    grep -E '=.*$' .config | grep -v -E '=y$' | sed 's/=/ /' | sed 's/^/#define /' >> include/linux/autoconf.h

    # Remove SHIPPED files
    find . -name "*_shipped" -delete
    # Remove binary blobs
    while read f; do
        rm $f
    done < bad-files
    ../../drop-blobs.sh
}

src_compile() {
    make ARCH=i386
}

src_install() {
    mkdir -p "${PREFIX}/boot"
    cp arch/i386/boot/bzImage "${PREFIX}/boot/linux-2.6.16.62"
}
