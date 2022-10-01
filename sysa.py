#!/usr/bin/env python3
"""System A"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>

import os
from distutils.dir_util import copy_tree
import shutil

from lib.sysgeneral import SysGeneral, stage0_arch_map

# pylint: disable=consider-using-with
class SysA(SysGeneral):
    """
    Class responsible for preparing sources for System A.
    """
    # pylint: disable=too-many-instance-attributes,too-many-arguments
    def __init__(self, arch, preserve_tmp, external_sources, tmpdir, sysb_dir, sysc_dir):
        self.git_dir = os.path.dirname(os.path.join(__file__))
        self.arch = arch
        self.preserve_tmp = preserve_tmp

        self.sys_dir = os.path.join(self.git_dir, 'sysa')
        if tmpdir is None:
            self.tmp_dir = os.path.join(self.git_dir, 'tmp')
        else:
            self.tmp_dir = os.path.join(tmpdir, 'sysa')
        self.sysa_dir = os.path.join(self.tmp_dir, 'sysa')
        self.base_dir = self.sysa_dir
        self.cache_dir = os.path.join(self.sys_dir, 'distfiles')
        self.sysb_dir = sysb_dir
        self.sysc_dir = sysc_dir
        self.external_sources = external_sources

    def prepare(self, mount_tmpfs, create_initramfs, repo_path):
        """
        Prepare directory structure for System A.
        We create an empty tmp directory, unpack stage0-posix.
        Rest of the files are unpacked into more structured directory /sysa
        """
        if mount_tmpfs:
            self.mount_tmpfs()
        else:
            os.mkdir(self.tmp_dir)

        self.stage0_posix()
        self.sysa()

        # sysb must be added to sysa as it is another initramfs stage
        self.sysb()

        self.sysc(create_initramfs)

        if repo_path:
            repo_dir = os.path.join(self.tmp_dir, 'usr', 'src', 'repo-preseeded')
            shutil.copytree(repo_path, repo_dir)

        if create_initramfs:
            self.make_initramfs()

    def sysa(self):
        """Copy in sysa files for sysa."""
        self.get_packages()

        shutil.copytree(self.sys_dir, os.path.join(self.tmp_dir, 'sysa'),
                ignore=shutil.ignore_patterns('tmp'))

    def sysb(self):
        """Copy in sysb files for sysb."""
        shutil.copytree(self.sysb_dir, os.path.join(self.tmp_dir, 'sysb'),
                ignore=shutil.ignore_patterns('tmp'))

    def sysc(self, create_initramfs):
        """Copy in sysc files for sysc."""
        if create_initramfs:
            ignore = shutil.ignore_patterns('tmp', 'distfiles')
        else:
            ignore = shutil.ignore_patterns('tmp')
        shutil.copytree(self.sysc_dir, os.path.join(self.tmp_dir, 'sysc'),
                ignore=ignore)

    def stage0_posix(self):
        """Copy in all of the stage0-posix"""
        stage0_posix_base_dir = os.path.join(self.sys_dir, 'stage0-posix', 'src')
        copy_tree(stage0_posix_base_dir, self.tmp_dir)

        arch = stage0_arch_map.get(self.arch, self.arch)
        kaem_optional_seed = os.path.join(self.sys_dir, 'stage0-posix', 'src', 'bootstrap-seeds',
                                          'POSIX', arch, 'kaem-optional-seed')
        shutil.copy2(kaem_optional_seed, os.path.join(self.tmp_dir, 'init'))

        # stage0-posix hook to continue running live-bootstrap
        shutil.copy2(os.path.join(self.sys_dir, 'after.kaem'),
                     os.path.join(self.tmp_dir, 'after.kaem'))
