#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -ex

# Build the ext2 image
make_fiwix_initrd -s 1376256 /boot/fiwix.ext2

# Boot Fiwix
if match x${BARE_METAL} xTrue; then
    kexec-fiwix /boot/fiwix -i /boot/fiwix.ext2 -m /e820 -c "fiwix console=/dev/tty1 root=/dev/ram0 initrd=fiwix.ext2 kexec_proto=linux kexec_size=204800 kexec_cmdline=\"init=/init consoleblank=0\""
else
    kexec-fiwix /boot/fiwix -i /boot/fiwix.ext2 -m /e820 -c "fiwix console=/dev/ttyS0 root=/dev/ram0 initrd=fiwix.ext2 kexec_proto=linux kexec_size=204800 kexec_cmdline=\"init=/init console=ttyS0\""
fi
