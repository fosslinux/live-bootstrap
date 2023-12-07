#!/bin/sh
#
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
dhcpcd --waitip=4
# Ensure network accessible
timeout=120
while ! curl example.com >/dev/null 2>&1; do
    sleep 1
    # shellcheck disable=SC2219
    let timeout--
    if [ "${timeout}" -le 0 ]; then
        echo "Timeout reached for internet to become accessible"
        false
    fi
done
