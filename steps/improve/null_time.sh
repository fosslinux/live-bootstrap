#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Set modified dates of all files to be 0 unix time.
# This function needs `touch` that supports --no-dereference
# (at least coreutils 8.1).
find / -xdev -exec touch --no-dereference -t 197001010000.00 {} +
