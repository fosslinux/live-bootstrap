# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

extract="linux-4.14.336/scripts linux-4.14.336/include linux-4.14.336/arch/x86/include linux-4.14.336/arch/x86/entry"

src_prepare() {
    default

    # Buggy headers/don't know how to account for
    rm include/uapi/linux/pktcdvd.h \
        include/uapi/linux/hw_breakpoint.h \
        include/uapi/linux/eventpoll.h \
        include/uapi/linux/atmdev.h \
        include/uapi/asm-generic/fcntl.h \
        arch/x86/include/uapi/asm/mman.h \
        arch/x86/include/uapi/asm/auxvec.h
}

src_compile() {
    gcc -o scripts/unifdef scripts/unifdef.c
}

src_install() {
    base_dir="${PWD}"
    # We "compile" the headers here because it is easier
    for d in include/uapi arch/x86/include/uapi; do
        cd "${d}"
        find . -type d -exec mkdir "${DESTDIR}${PREFIX}/include/{}" -p \;
        headers="$(find . -type f -name "*.h")"
        cd "${base_dir}"
        for h in ${headers}; do
            path="$(dirname "${h}")"
            scripts/headers_install.sh "${DESTDIR}${PREFIX}/include/${path}" "${d}/${path}" "$(basename "${h}")"
        done
    done

    # Pick-and-choose asm-generic headers
    for i in types ioctl termios termbits ioctls sockios socket param; do
        cp "${DESTDIR}${PREFIX}/include/asm-generic/${i}.h" "${DESTDIR}${PREFIX}/include/asm/${i}.h"
    done

    # Generate asm/unistd_32.h
    bash arch/x86/entry/syscalls/syscallhdr.sh \
        arch/x86/entry/syscalls/syscall_32.tbl \
        "${DESTDIR}${PREFIX}/include/asm/unistd_32.h" i386

    # Generate linux/version.h
    # Rules are from makefile
    VERSION=4
    PATCHLEVEL=14
    SUBLEVEL=336
    VERSION_CODE="$((VERSION * 65536 + PATCHLEVEL * 256 + SUBLEVEL))"
    echo '#define LINUX_VERSION_CODE '"${VERSION_CODE}" \
        > "${DESTDIR}${PREFIX}/include/linux/version.h"
    echo '#define KERNEL_VERSION(a,b,c) (((a) << 16) + ((b) << 8) + ((c) > 255 ? 255 : (c)))' \
        >> "${DESTDIR}${PREFIX}/include/linux/version.h"

    # Clear up storage space
    cd ../..
    rm -rf build src
}
