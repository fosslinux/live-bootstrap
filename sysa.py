#!/usr/bin/env python3
"""System A"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022-2023 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>

import os
from distutils.dir_util import copy_tree
import shutil
import tarfile

from lib.sysgeneral import SysGeneral, stage0_arch_map

# pylint: disable=consider-using-with
# pylint: disable=too-many-instance-attributes
class SysA(SysGeneral):
    """
    Class responsible for preparing sources for System A.
    """

    git_dir = os.path.dirname(os.path.join(__file__))
    sys_dir = os.path.join(git_dir, 'sysa')
    sysb_dir = os.path.join(git_dir, 'sysb')
    sysc_dir = os.path.join(git_dir, 'sysc')
    cache_dir = os.path.join(sys_dir, 'distfiles')

    # pylint: disable=too-many-arguments
    def __init__(self, tmpdir, arch, external_sources,
                 early_preseed, repo_path):
        self.arch = arch
        self.early_preseed = early_preseed
        self.external_sources = external_sources
        self.repo_path = repo_path

        self.tmp_dir = tmpdir.add_sys("sysa")

    def prepare(self, create_initramfs):
        """
        Prepare directory structure for System A.
        We create an empty tmp directory, unpack stage0-posix.
        Rest of the files are unpacked into more structured directory /sysa
        """
        if self.early_preseed:
            # Extract tar containing preseed
            with tarfile.open(self.early_preseed, "r") as seed:
                seed.extractall(self.tmp_dir)
            shutil.copy2(os.path.join(self.sys_dir, 'base-preseeded.kaem'),
                         os.path.join(self.tmp_dir, 'kaem.x86'))
        else:
            self.stage0_posix()

        self.sysa()

        # sysb must be added to sysa as it is another initramfs stage
        self.sysb()

        self.sysc(create_initramfs)

        if self.repo_path:
            repo_dir = os.path.join(self.tmp_dir, 'usr', 'src', 'repo-preseeded')
            shutil.copytree(self.repo_path, repo_dir)

        if create_initramfs:
            self.make_initramfs()

    def sysa(self):
        """Copy in sysa files for sysa."""
        source_manifest = self.get_source_manifest()
        self.get_packages(source_manifest)

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
