#!/bin/sh

# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

sed -i "/^LIBDIR/d" /steps/env
TARGET=i686-unknown-linux-musl
LIBDIR=${PREFIX}/lib/${TARGET}
echo "LIBDIR=${LIBDIR}" >> /steps/env
echo "TARGET=${TARGET}" >> /steps/env
