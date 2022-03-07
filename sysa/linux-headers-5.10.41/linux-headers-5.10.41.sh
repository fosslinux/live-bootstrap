# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    src_dir="${base_dir}/src"
    tar -xzf "${src_dir}/"*.tar.gz "${dirname}/scripts"
    tar -xzf "${src_dir}/"*.tar.gz "${dirname}/include"
    tar -xzf "${src_dir}/"*.tar.gz "${dirname}/arch/x86/include"
    tar -xzf "${src_dir}/"*.tar.gz "${dirname}/arch/x86/entry"
}

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
    export base_dir="${PWD}"
    # We "compile" the headers here because it is easier
    for d in include/uapi arch/x86/include/uapi; do
        cd "${d}"
        find . -type d -exec mkdir "${DESTDIR}${PREFIX}/include/{}" -p \;
        headers="$(find . -type f -name "*.h")"
        cd "${base_dir}"
        for h in ${headers}; do
            scripts/headers_install.sh "${d}/${h}" "${DESTDIR}${PREFIX}/include/${h}"
        done
    done

    # Pick-and-choose asm-generic headers
    for i in types ioctl termios termbits ioctls; do
        cp "${DESTDIR}${PREFIX}/include/asm-generic/${i}.h" "${DESTDIR}${PREFIX}/include/asm/${i}.h"
    done

    # Generate asm/unistd_32.h
    bash arch/x86/entry/syscalls/syscallhdr.sh \
        arch/x86/entry/syscalls/syscall_32.tbl \
        "${DESTDIR}${PREFIX}/include/asm/unistd_32.h" i386

    # Generate linux/version.h
    # Rules are from makefile
    VERSION=5
    PATCHLEVEL=10
    SUBLEVEL=42
    VERSION_CODE="$((${VERSION} * 65536 + ${PATCHLEVEL} * 256 + ${SUBLEVEL}))"
    echo '#define LINUX_VERSION_CODE '"${VERSION_CODE}" \
        > "${DESTDIR}${PREFIX}/include/linux/version.h"
    echo '#define KERNEL_VERSION(a,b,c) (((a) << 16) + ((b) << 8) + ((c) > 255 ? 255 : (c)))' \
        >> "${DESTDIR}${PREFIX}/include/linux/version.h"

    # Clear up storage space
    cd ../..
    rm -rf build src
}
