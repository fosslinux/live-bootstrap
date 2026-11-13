#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -ex

# Build the ext2 image
# 1392640 = 1360 MB
make_fiwix_initrd -s 1381376 /boot/fiwix.ext2

# Boot Fiwix
# 199680 = 195 MB
# as of 2024-05-27, Initrd = ~183 MB, kernel = ~10.5MB for Linux
if match x${BARE_METAL} xTrue; then
    kexec-fiwix /boot/fiwix -i /boot/fiwix.ext2 -m /e820 -c "fiwix console=/dev/tty1 root=/dev/ram0 initrd=fiwix.ext2 kexec_proto=linux kexec_size=199680 kexec_cmdline=\"init=/init consoleblank=0\""
else
    kexec-fiwix /boot/fiwix -i /boot/fiwix.ext2 -m /e820 -c "fiwix console=/dev/ttyS0 root=/dev/ram0 initrd=fiwix.ext2 kexec_proto=linux kexec_size=199680 kexec_cmdline=\"init=/init console=ttyS0,115200\""
fi
