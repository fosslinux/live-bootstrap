#!/usr/bin/env python3
"""System B"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>

import os

from lib.sysgeneral import SysGeneral

class SysB(SysGeneral):
    """
    Class responsible for preparing sources for System B.
    """
    def __init__(self, arch, preserve_tmp, chroot):
        self.git_dir = os.path.dirname(os.path.join(__file__))
        self.arch = arch
        self.preserve_tmp = preserve_tmp
        self.chroot = chroot

        self.sys_dir = os.path.join(self.git_dir, 'sysb')
