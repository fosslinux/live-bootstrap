#!/usr/bin/env python3
"""System B"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

import os
import shutil

from lib.utils import copytree
from lib.sysgeneral import SysGeneral

class SysB(SysGeneral):
    """
    Class responsible for preparing sources for System B.
    """
    def __init__(self, arch, preserve_tmp, tmpdir, chroot):
        self.git_dir = os.path.dirname(os.path.join(__file__))
        self.arch = arch
        self.preserve_tmp = preserve_tmp
        self.chroot = chroot

        self.sys_dir = os.path.join(self.git_dir, 'sysb')
        if tmpdir is None:
            self.tmp_dir = os.path.join(self.sys_dir, 'tmp')
        else:
            self.tmp_dir = os.path.join(tmpdir, 'sysb')
            os.mkdir(self.tmp_dir)

        self.prepare()

    def prepare(self):
        """
        Prepare directory structure for System B.
        """
        self.mount_tmpfs()
        copytree(self.sys_dir, self.tmp_dir, ignore=shutil.ignore_patterns("tmp"))
