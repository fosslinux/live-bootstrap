#!/bin/sh
#
# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Finalize job count once SMP support is available

cat >> /steps/bootstrap.cfg <<- EOF
JOBS=${FINAL_JOBS}
EOF

. /steps/bootstrap.cfg
. /steps/env
