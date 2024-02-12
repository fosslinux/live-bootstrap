#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
mkdir -p /external/repo

tar -cf - --exclude='/external/repo/*' --exclude='/external/repo-preseeded/*' --exclude='/external/distfiles/*' --exclude='/dev/*' --exclude='/proc/*' --exclude='/sys/*' --exclude='/tmp/*' / | bzip2 --best > /external/repo/base.tar.bz2
