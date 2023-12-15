#!/bin/sh

# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

sed -i "/^LIBDIR/d" /steps/env
LIBDIR=${PREFIX}/lib/i386-unknown-linux-musl
echo "LIBDIR=${LIBDIR}" >> /steps/env
