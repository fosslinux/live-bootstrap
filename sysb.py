#!/usr/bin/env python3
"""System B"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

import os
import shutil

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
        self.base_dir = os.path.join(self.tmp_dir, 'usr', 'src')

        self.prepare()

    def prepare(self):
        """
        Prepare directory structure for System B.
        """
        self.mount_tmpfs()
        os.makedirs(self.base_dir)

        # Misc files/scripts
        self.deploy_sysglobal_files()
        self.deploy_scripts()

    def deploy_scripts(self):
        """Add the scripts to the chroot"""
        # init script
        shutil.copy2(os.path.join(self.sys_dir, 'init'), self.tmp_dir)
        # run.sh
        shutil.copy2(os.path.join(self.sys_dir, 'run.sh'), self.base_dir)
