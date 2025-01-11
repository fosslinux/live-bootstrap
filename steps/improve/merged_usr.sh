#!/bin/sh
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Add the rest of the FHS that we will use and is not created pre-boot
ln -s bin /usr/sbin
for d in bin lib sbin; do
    if [ -d "/${d}" ] && ! [ -L "/${d}" ]; then
        # Move the non symlink directory out of the way
        mv "/${d}" "/${d}-saved"
    fi
    ln -s "usr/${d}" "/${d}" || true # these might exist if rerunning
done
