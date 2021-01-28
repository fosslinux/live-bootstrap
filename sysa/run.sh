#!/bin/bash

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-License-Identifier: GPL-3.0-or-later

set -e
# shellcheck source=sysa/helpers.sh
. helpers.sh

export PREFIX=/after

# Part 21
build m4-1.4

# Part 22
build flex-2.5.11
build flex-2.5.11

echo "Bootstrapping completed."
