# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Delete build artifacts to free up space for Linux kernel build

for pkg in $(ls "${SRCDIR}"); do
    if [ -d "${SRCDIR}/${pkg}/build" ]; then
        rm -rf "${SRCDIR}/${pkg}/build"
    fi
done

rm -rf "/${ARCH_DIR}/artifact"
