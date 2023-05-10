#!/usr/bin/env python3
"""System A"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022-2023 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>

import os
# pylint: disable=deprecated-module
from distutils.dir_util import copy_tree
import shutil
import tarfile

from lib.sysgeneral import SysGeneral, stage0_arch_map
from lib.utils import run

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

    def prepare(self, create_initramfs, kernel_bootstrap=False):
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

        if kernel_bootstrap:
            self.create_fiwix_file_list()
            self.create_builder_hex0_disk_image(os.path.join(self.tmp_dir, 'sysa.img'))
            return

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
        if create_initramfs or not self.external_sources:
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

    def add_fiwix_files(self, file_list_path, dirpath):
        """Add files to the list to populate Fiwix file system"""
        for root, _, filepaths in os.walk(dirpath):
            if 'stage0-posix' in root:
                continue
            with open(file_list_path, 'a', encoding="utf-8") as file_list:
                for filepath in filepaths:
                    file_list.write(f"/{os.path.join(root, filepath)}\n")

    def create_fiwix_file_list(self):
        """Create a list of files to populate Fiwix file system"""
        file_list_path = os.path.join(self.tmp_dir, 'sysa', 'lwext4-1.0.0-lb1',
                                      'files', 'fiwix-file-list.txt')
        shutil.copyfile(os.path.join(self.tmp_dir, 'sysa', 'lwext4-1.0.0-lb1',
                                     'files', 'early-artifacts-needed-after-fiwix.txt'),
                        file_list_path)

        save_cwd = os.getcwd()
        self.add_fiwix_files(file_list_path, 'sysa')
        self.add_fiwix_files(file_list_path, 'sysb')
        self.add_fiwix_files(file_list_path, 'sysc')
        os.chdir(save_cwd)

    @staticmethod
    def output_dir(srcfs_file, dirpath):
        """Add a directory to srcfs file system"""
        srcline = f"src 0 {dirpath}\n"
        srcfs_file.write(srcline.encode())

    @staticmethod
    def output_file(srcfs_file, filepath):
        """Add a file to srcfs file system"""
        srcline = f"src {os.path.getsize(filepath)} {filepath}\n"
        srcfs_file.write(srcline.encode())
        with open(filepath, 'rb') as srcfile:
            srcfs_file.write(srcfile.read())

    def output_tree(self, srcfs_file, treepath):
        """Add a tree of files to srcfs file system"""
        self.output_dir(srcfs_file, treepath)
        for root, dirs, files in os.walk(treepath):
            if ".git" in root:
                continue
            for dirpath in dirs:
                if ".git" in dirpath:
                    continue
                self.output_dir(srcfs_file, os.path.join(root, dirpath))

            for filepath in files:
                if ".git" in filepath:
                    continue
                self.output_file(srcfs_file, os.path.join(root, filepath))

    def append_srcfs(self, image_file):
        """Append srcfs file system to sysa disk image"""
        save_cwd = os.getcwd()

        os.chdir(os.path.join(self.tmp_dir, 'sysa', 'stage0-posix', 'src'))
        self.output_tree(image_file, '.')

        os.chdir(self.tmp_dir)
        shutil.move(os.path.join('sysa', 'stage0-posix'), '.')
        self.output_tree(image_file, 'sysa')
        self.output_tree(image_file, 'sysb')
        self.output_tree(image_file, 'sysc')
        shutil.move('stage0-posix', 'sysa')
        shutil.copyfile(os.path.join('sysa', 'after.kaem'), 'after.kaem')
        self.output_file(image_file, 'after.kaem')

        # Add commands to kick off stage0-posix
        cmd = ' '.join(['hex0',
                        './bootstrap-seeds/POSIX/x86/hex0_x86.hex0'
                        './bootstrap-seeds/POSIX/x86/hex0-seed\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['hex0',
                        './bootstrap-seeds/POSIX/x86/kaem-minimal.hex0',
                        './bootstrap-seeds/POSIX/x86/kaem-optional-seed\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['./bootstrap-seeds/POSIX/x86/kaem-optional-seed', './kaem.x86\n'])
        image_file.write(cmd.encode())

        os.chdir(save_cwd)

    def create_builder_hex0_disk_image(self, image_file_name):
        """Create builder-hex0 disk image"""
        shutil.copyfile(os.path.join('kernel-bootstrap', 'builder-hex0-x86-stage1.bin'), image_file_name)

        with open(image_file_name, 'ab') as image_file:
            current_size = os.stat(image_file_name).st_size
            while current_size != 510:
                image_file.write(b'\0')
                current_size += 1
            image_file.write(b'\x55')
            image_file.write(b'\xAA')

            # Append stage2 hex0 source
            with open(os.path.join('kernel-bootstrap', 'builder-hex0-x86-stage2.hex0')) as infile:
                image_file.write(infile.read().encode())
            # Pad to next sector
            current_size = os.stat(image_file_name).st_size
            while current_size % 512 != 0:
                image_file.write(b'\0')
                current_size += 1
            self.append_srcfs(image_file)

        current_size = os.stat(image_file_name).st_size

        megabyte = 1024 * 1024
        # fill file with zeros up to next megabyte
        extra = current_size % megabyte
        round_up = megabyte - extra
        with open(image_file_name, 'ab') as image_file:
            image_file.write(b'\0' * round_up)
        current_size += round_up

        # fill file with zeros up to desired size, one megabyte at a time
        with open(image_file_name, 'ab') as image_file:
            while current_size < 16384 * megabyte:
                image_file.write(b'\0' * megabyte)
                current_size += megabyte
