#!/bin/sh -e
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# This file replaces keywords.pl

sed -e '1,/__END__/ d' keywords.pl | sed '1d' | awk '{print "#define", "KEY_"$0, NR-1}' > keywords.h
