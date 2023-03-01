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

    def find_tree(self, dirpath):
        subdirs, files = [], []

        for f in os.scandir(dirpath):
            if f.is_dir():
                subdirs.append(f.path)
            if f.is_file():
                files.append(f.path)

        for subdir in list(subdirs):
            more_dirs, more_files = self.find_tree(subdir)
            subdirs.extend(more_dirs)
            files.extend(more_files)

        return subdirs, files

    def add_fiwix_files(self, file_list_path, dirpath):
        dirpaths, filepaths = self.find_tree(dirpath)
        with open(file_list_path, 'a') as file_list:
            for filepath in filepaths:
                if 'stage0-posix' in filepath:
                    continue
                file_list.write("/%s\n" % filepath)

    def create_fiwix_file_list(self):
        file_list_path = os.path.join(self.tmp_dir, 'sysa', 'lwext4-1.0.0-lb1', 'files', 'fiwix-file-list.txt')
        shutil.copyfile(os.path.join(self.tmp_dir, 'sysa', 'lwext4-1.0.0-lb1', 'files', 'early-artifacts-needed-after-fiwix.txt'),
                file_list_path)

        save_cwd = os.getcwd()
        self.add_fiwix_files(file_list_path, 'sysa')
        self.add_fiwix_files(file_list_path, 'sysb')
        self.add_fiwix_files(file_list_path, 'sysc')
        os.chdir(save_cwd)

    def output_dir(self, srcfs, dirpath):
        srcline = "src 0 %s\n" % dirpath
        srcfs.write(srcline.encode())

    def output_file(self, srcfs, filepath):
        srcline = "src %d %s\n" % (os.path.getsize(filepath), filepath)
        srcfs.write(srcline.encode())
        with open(filepath, 'rb') as srcfile:
            srcfs.write(srcfile.read())

    def append_srcfs(self, srcfs):
        self.output_dir(srcfs, '/')

        save_cwd = os.getcwd()

        os.chdir(os.path.join(self.tmp_dir, 'sysa', 'stage0-posix', 'src'))
        dirs, files = self.find_tree('.')
        for dirpath in dirs:
            if ".git" in dirpath:
                continue
            self.output_dir(srcfs, dirpath)

        for filepath in files:
            if ".git" in filepath:
                continue
            self.output_file(srcfs, filepath)

        os.chdir(self.tmp_dir)
        shutil.move(os.path.join('sysa','stage0-posix'), '.')
        shutil.move(os.path.join('sysa', 'distfiles'), '.')
        run('tar', '-c', '-z', '-p',
            '--dereference', '--hard-dereference',
            '-f', os.path.join('..', 'live-bootstrap.tar.gz'),
            'sysa', 'sysb', 'sysc')
        shutil.move('stage0-posix', 'sysa')
        shutil.move('distfiles', 'sysa')

        os.chdir('..')
        self.output_file(srcfs, 'live-bootstrap.tar.gz')

        os.chdir(self.tmp_dir)

        # We need to have enough to start live-bootstrap without the
        # live-bootstrap tar file which is not extracted right away.
        shutil.copyfile(os.path.join('sysa', 'after.kaem'), 'after.kaem')
        self.output_file(srcfs, 'after.kaem')
        self.output_dir(srcfs, 'sysa')
        self.output_file(srcfs, os.path.join('sysa', 'run.kaem'))
        self.output_file(srcfs, os.path.join('sysa', 'bootstrap.cfg'))

        self.output_dir(srcfs, os.path.join('sysa', 'distfiles'))
        dirs, files = self.find_tree(os.path.join('sysa', 'distfiles'))
        for filepath in files:
            self.output_file(srcfs, filepath)

        # Add commands to kick off stage0-posix
        srcfs.write("hex0 ./bootstrap-seeds/POSIX/x86/hex0_x86.hex0 ./bootstrap-seeds/POSIX/x86/hex0-seed\n".encode())
        srcfs.write("hex0 ./bootstrap-seeds/POSIX/x86/kaem-minimal.hex0 ./bootstrap-seeds/POSIX/x86/kaem-optional-seed\n".encode())
        srcfs.write("./bootstrap-seeds/POSIX/x86/kaem-optional-seed ./kaem.x86\n".encode())

        os.chdir(save_cwd)

    def create_builder_hex0_disk_image(self, image_file):

        run(os.path.join('sysa', 'stage0-posix', 'src', 'bootstrap-seeds', 'POSIX', 'x86', 'hex0-seed'),
            os.path.join('kernel-bootstrap', 'builder-hex0-x86.hex0'),
            image_file)

        with open(image_file, 'ab') as srcfs:
            self.append_srcfs(srcfs)

        current_size = os.stat(image_file).st_size

        MB = 1024 * 1024
        # fill file with zeros up to next megabyte
        extra = current_size % MB
        round_up = MB - extra
        with open(image_file, 'ab') as srcfs:
            srcfs.write(b'\0' * round_up)
        current_size += round_up

        # fill file with zeros up to desired size, one megabyte at a time
        with open(image_file, 'ab') as srcfs:
            while current_size < 1008 * MB:
                srcfs.write(b'\0' * MB)
                current_size += MB
