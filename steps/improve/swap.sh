#!/bin/sh
#
# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Set up swap

. /steps/bootstrap.cfg
. /steps/env

if ! test -f /swapfile
then
    echo "Making swap..."
    dd if=/dev/zero of=/swapfile bs=1M count=${SWAP_SIZE}
    mkswap /swapfile
fi
swapon /swapfile
