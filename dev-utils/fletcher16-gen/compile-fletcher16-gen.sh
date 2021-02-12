#!/bin/sh

# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -ex

M2-Planet \
    --architecture x86 \
    -f ../m2-functions/in_set.c \
    -f ../m2-functions/file_print.c \
    -f ../m2-functions/numerate_number.c \
    -f ../m2-functions/string.c \
    -f ../../sysa/mescc-tools-seed/src/mescc-tools-seed/M2-Planet/test/common_x86/functions/file.c \
    -f ../../sysa/mescc-tools-seed/src/mescc-tools-seed/M2-Planet/test/common_x86/functions/exit.c \
    -f ../m2-functions/require.c \
    -f ../../sysa/mescc-tools-seed/src/mescc-tools-seed/M2-Planet/test/common_x86/functions/malloc.c \
    -f ../../sysa/mescc-tools-seed/src/mescc-tools-seed/M2-Planet/functions/calloc.c \
    -f fletcher16-gen.c \
    -o fletcher16.M1 \
    --debug

blood-elf -f fletcher16.M1 -o fletcher16-footer.M1

M1 \
    -f ../../sysa/mescc-tools-seed/src/mescc-tools-seed/M2-Planet/test/common_x86/x86_defs.M1 \
    -f ../../sysa/mescc-tools-seed/src/mescc-tools-seed/M2-Planet/test/common_x86/libc-core.M1 \
    -f fletcher16.M1 \
    -f fletcher16-footer.M1 \
    --LittleEndian \
    --architecture x86 \
    -o fletcher16.hex2

hex2 \
    -f ../../sysa/mescc-tools-seed/src/mescc-tools-seed/M2-Planet/test/common_x86/ELF-i386-debug.hex2 \
    -f fletcher16.hex2 \
    --LittleEndian \
    --architecture x86 \
    --BaseAddress 0x8048000 \
    -o fletcher16-gen \
    --exec_enable
