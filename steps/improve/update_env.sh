#!/bin/sh

unset GUILE_LOAD_PATH

cat > /steps/env <<- EOF
export PATH=${PREFIX}/bin
PREFIX=${PREFIX}
LIBDIR=${LIBDIR}
DESTDIR=${DESTDIR}
DISTFILES=${DISTFILES}
SRCDIR=${SRCDIR}
MAKEJOBS=-j${JOBS}
export HOME=/tmp
export SOURCE_DATE_EPOCH=0
export SHELL=/usr/bin/bash
DESTDIR=/tmp/destdir
EOF

. /steps/env
