#!/bin/sh

# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

if bash --version | grep -q 'GPLv3'; then
    env - PATH=${PREFIX}/bin PS1="\w # " openvt -- bash -i
else
    bash -c 'while true; do printf "[early Bash - use Ctrl+D] $(pwd) # "; eval "$(cat /dev/tty2)"; done' &> /dev/tty2 &
fi
