#!/bin/sh

# SPDX-FileCopyrightText: 2023 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

unset GUILE_LOAD_PATH

cat >> /steps/env <<- 'EOF'
export PATH=${PREFIX}/bin
PREFIX=${PREFIX}
LIBDIR=${LIBDIR}
DESTDIR=${DESTDIR}
DISTFILES=${DISTFILES}
SRCDIR=${SRCDIR}
MAKEJOBS=-j${JOBS}
export HOME=/tmp
export SOURCE_DATE_EPOCH=0
export KBUILD_BUILD_TIMESTAMP='@0'
export SHELL=/usr/bin/bash
DESTDIR=/tmp/destdir
EOF

# The following values are set up in the kaem environment.
# As these are then passed through to the bash shell, they are considered
# automatically exported variables. We don't want them exported.
unset PREFIX
unset BINDIR
unset LIBDIR
unset INCDIR
unset SRCDIR
unset TMPDIR
unset DISTFILES

. /steps/env
