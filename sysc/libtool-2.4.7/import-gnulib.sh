#!/bin/sh

# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

../gnulib-a521820/gnulib-tool \
    --no-changelog \
    --avoid=dummy \
    --libtool \
    --macro-prefix=GL \
    --with-tests \
    --tests-base=gnulib-tests \
    --aux-dir=build-aux \
    --m4-base=m4 \
    --local-dir=gl \
    --local-dir=gl-mod/bootstrap \
    --symlink \
    --import announce-gen \
        bootstrap \
        do-release-commit-and-tag \
        extract-trace \
        gendocs \
        git-version-gen \
        gitlog-to-changelog \
        gnu-web-doc-update \
        gnupload \
        inline-source \
        maintainer-makefile \
        options-parser \
        readme-release \
        update-copyright
