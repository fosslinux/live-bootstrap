#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -ex

# Build the ext2 image
make_fiwix_initrd

# Boot Fiwix
kexec-fiwix
