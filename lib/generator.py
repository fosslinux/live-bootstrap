#!/usr/bin/env python3
"""
This file contains all code required to generate the boot image for live-bootstrap
"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022-2023 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2021-23 fosslinux <fosslinux@aussies.space>

import hashlib
import os
import shutil
import tarfile
import requests

# pylint: disable=too-many-instance-attributes
class Generator():
    """
    Class responsible for generating the basic media to be consumed.
    """

    git_dir = os.path.join(os.path.dirname(os.path.join(__file__)), '..')
    distfiles_dir = os.path.join(git_dir, 'distfiles')

    def __init__(self, arch, external_sources, early_preseed, repo_path):
        self.arch = arch
        self.early_preseed = early_preseed
        self.external_sources = external_sources
        self.repo_path = repo_path
        self.source_manifest = self.get_source_manifest(not self.external_sources)
        self.target_dir = None
        self.external_dir = None

    def reuse(self, target):
        """
        Reuse a previously prepared bwrap environment for further stages.
        """
        self.target_dir = target.path
        self.external_dir = os.path.join(self.target_dir, 'external')
        self.distfiles()

    def prepare(self, target, using_kernel=False, kernel_bootstrap=False, target_size=0):
        """
        Prepare basic media of live-bootstrap.
        /steps -- contains steps to be built
        / -- contains seed to allow steps to be built, containing custom
             scripts and stage0-posix
        """
        self.target_dir = target.path
        self.external_dir = os.path.join(self.target_dir, 'external')

        # We use ext3 here; ext4 actually has a variety of extensions that
        # have been added with varying levels of recency
        # Linux 4.9.10 does not support a bunch of them
        # Attempting to disable extensions that a particular e2fsprogs
        # is *unaware* of causes the filesystem creation to fail
        # We could hypothetically detect e2fsprogs version and create an
        # argument matrix ... or we could just use ext3 instead which
        # is effectively universally the same
        if kernel_bootstrap:
            init_path = os.path.join(self.target_dir, 'init')

            os.mkdir(init_path)
            self.target_dir = init_path

            if self.repo_path or self.external_sources:
                target.add_disk("external", filesystem="ext3")
                target.mount_disk("external", "external")
            else:
                self.external_dir = os.path.join(self.target_dir, 'external')
        elif using_kernel:
            self.target_dir = os.path.join(self.target_dir, 'disk')
            target.add_disk("disk",
                            filesystem="ext3",
                            size=(str(target_size) + "M") if target_size else "16G",
                            bootable=True)
            target.mount_disk("disk", "disk")
            self.external_dir = os.path.join(self.target_dir, 'external')

        os.makedirs(self.external_dir, exist_ok=True)

        if self.early_preseed:
            # Extract tar containing preseed
            with tarfile.open(self.early_preseed, "r") as seed:
                seed.extractall(self.target_dir)
            shutil.copy2(os.path.join(self.git_dir, 'seed', 'preseeded.kaem'),
                         os.path.join(self.target_dir, 'kaem.x86'))
        else:
            self.stage0_posix()
            self.seed()

        self.steps()

        self.distfiles()

        self.create_fiwix_file_list()

        if self.repo_path:
            repo_dir = os.path.join(self.external_dir, 'repo-preseeded')
            shutil.copytree(self.repo_path, repo_dir)

        if kernel_bootstrap:
            self.create_builder_hex0_disk_image(self.target_dir + '.img', target_size)

        if kernel_bootstrap and (self.external_sources or self.repo_path):
            target.umount_disk('external')
        elif using_kernel:
            target.umount_disk('disk')

    def steps(self):
        """Copy in steps."""
        self.get_packages()

        shutil.copytree(os.path.join(self.git_dir, 'steps'), os.path.join(self.target_dir, 'steps'))

    def stage0_posix(self):
        """Copy in all of the stage0-posix"""
        stage0_posix_base_dir = os.path.join(self.git_dir, 'seed', 'stage0-posix')
        for entry in os.listdir(stage0_posix_base_dir):
            orig = os.path.join(stage0_posix_base_dir, entry)
            target = os.path.join(self.target_dir, entry)
            if os.path.isfile(orig):
                shutil.copy2(orig, target)
            else:
                shutil.copytree(orig, target)

        arch = stage0_arch_map.get(self.arch, self.arch)
        kaem_optional_seed = os.path.join(self.git_dir, 'seed', 'stage0-posix', 'bootstrap-seeds',
                                          'POSIX', arch, 'kaem-optional-seed')
        shutil.copy2(kaem_optional_seed, os.path.join(self.target_dir, 'init'))

    def seed(self):
        """Copy in extra seed files"""
        seed_dir = os.path.join(self.git_dir, 'seed')
        for entry in os.listdir(seed_dir):
            if os.path.isfile(os.path.join(seed_dir, entry)):
                shutil.copy2(os.path.join(seed_dir, entry), os.path.join(self.target_dir, entry))

    @staticmethod
    def add_fiwix_files(file_list_path, dirpath):
        """Add files to the list to populate Fiwix file system"""
        for root, _, filepaths in os.walk(dirpath):
            if 'stage0-posix' in root:
                continue
            with open(file_list_path, 'a', encoding="utf-8") as file_list:
                for filepath in filepaths:
                    file_list.write(f"/{os.path.join(root, filepath)}\n")

    def create_fiwix_file_list(self):
        """Create a list of files to populate Fiwix file system"""
        file_list_path = os.path.join(self.target_dir, 'steps', 'lwext4-1.0.0-lb1',
                                      'files', 'fiwix-file-list.txt')
        shutil.copyfile(os.path.join(self.target_dir, 'steps', 'lwext4-1.0.0-lb1',
                                     'files', 'early-artifacts-needed-after-fiwix.txt'),
                        file_list_path)

        save_cwd = os.getcwd()
        os.chdir(self.target_dir)
        self.add_fiwix_files(file_list_path, 'steps')
        self.add_fiwix_files(file_list_path, 'distfiles')
        os.chdir(save_cwd)

    def distfiles(self):
        """Copy in distfiles"""
        def copy_no_network_distfiles(out):
            # Note that "no disk" implies "no network" for kernel bootstrap mode
            for file in self.source_manifest:
                file = file[3].strip()
                shutil.copy2(os.path.join(self.distfiles_dir, file),
                             os.path.join(out, file))

        early_distfile_dir = os.path.join(self.target_dir, 'external', 'distfiles')
        main_distfile_dir = os.path.join(self.external_dir, 'distfiles')

        if early_distfile_dir != main_distfile_dir:
            os.makedirs(early_distfile_dir, exist_ok=True)
            copy_no_network_distfiles(early_distfile_dir)

        if self.external_sources:
            shutil.copytree(self.distfiles_dir, main_distfile_dir, dirs_exist_ok=True)
        else:
            os.mkdir(main_distfile_dir)
            copy_no_network_distfiles(main_distfile_dir)

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
        """Append srcfs file system to disk image"""
        save_cwd = os.getcwd()

        os.chdir(self.target_dir)
        self.output_tree(image_file, '.')

        # Add commands to kick off stage0-posix
        cmd = ' '.join(['hex0',
                        './bootstrap-seeds/POSIX/x86/hex0_x86.hex0',
                        './bootstrap-seeds/POSIX/x86/hex0-seed\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['hex0',
                        './bootstrap-seeds/POSIX/x86/kaem-minimal.hex0',
                        './bootstrap-seeds/POSIX/x86/kaem-optional-seed\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['./bootstrap-seeds/POSIX/x86/kaem-optional-seed', './kaem.x86\n'])
        image_file.write(cmd.encode())

        os.chdir(save_cwd)

    def create_builder_hex0_disk_image(self, image_file_name, size):
        """Create builder-hex0 disk image"""
        shutil.copyfile(os.path.join('seed', 'stage0-posix', 'bootstrap-seeds',
                                     'NATIVE', 'x86', 'builder-hex0-x86-stage1.img'),
                        image_file_name)

        with open(image_file_name, 'ab') as image_file:
            # Append stage2 hex0 source
            with open(os.path.join('kernel-bootstrap', 'builder-hex0-x86-stage2.hex0'),
                      encoding="utf-8") as infile:
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

        # extend file up to desired size
        if current_size < size * megabyte:
            with open(image_file_name, 'ab') as image_file:
                image_file.truncate(size * megabyte)

    @staticmethod
    def check_file(file_name, expected_hash):
        """Check hash of downloaded source file."""
        with open(file_name, "rb") as downloaded_file:
            downloaded_content = downloaded_file.read() # read entire file as bytes
        readable_hash = hashlib.sha256(downloaded_content).hexdigest()
        if expected_hash == readable_hash:
            return
        raise ValueError(f"Checksum mismatch for file {os.path.basename(file_name)}:\n\
expected: {expected_hash}\n\
actual:   {readable_hash}\n\
When in doubt, try deleting the file in question -- it will be downloaded again when running \
this script the next time")

    @staticmethod
    def download_file(url, directory, file_name):
        """
        Download a single source archive.
        """
        abs_file_name = os.path.join(directory, file_name)

        # Create a directory for downloaded file
        if not os.path.isdir(directory):
            os.mkdir(directory)

        # Actually download the file
        headers = {
                "Accept-Encoding": "identity"
        }
        if not os.path.isfile(abs_file_name):
            print(f"Downloading: {file_name}")
            response = requests.get(url, allow_redirects=True, stream=True,
                    headers=headers, timeout=20)
            if response.status_code == 200:
                with open(abs_file_name, 'wb') as target_file:
                    target_file.write(response.raw.read())
            else:
                raise requests.HTTPError("Download failed: HTTP " +
                    response.status_code + " " + response.reason)
        return abs_file_name

    def get_packages(self):
        """Prepare remaining sources"""
        for line in self.source_manifest:
            path = self.download_file(line[2], line[1], line[3])
            self.check_file(path, line[0])

    @classmethod
    def get_source_manifest(cls, pre_network=False):
        """
        Generate a source manifest for the system.
        """
        entries = []
        directory = os.path.relpath(cls.distfiles_dir, cls.git_dir)

        # Find all source files
        steps_dir = os.path.join(cls.git_dir, 'steps')
        with open(os.path.join(steps_dir, 'manifest'), 'r', encoding="utf_8") as file:
            for line in file:
                if pre_network and line.strip().startswith("improve: ") and "network" in line:
                    break

                if not line.strip().startswith("build: "):
                    continue

                step = line.split(" ")[1].split("#")[0].strip()
                sourcef = os.path.join(steps_dir, step, "sources")
                if os.path.exists(sourcef):
                    # Read sources from the source file
                    with open(sourcef, "r", encoding="utf_8") as sources:
                        for source in sources.readlines():
                            source = source.strip().split(" ")

                            if len(source) > 2:
                                file_name = source[2]
                            else:
                                # Automatically determine file name based on URL.
                                file_name = os.path.basename(source[0])

                            entry = (source[1], directory, source[0], file_name)
                            if entry not in entries:
                                entries.append(entry)

        return entries

stage0_arch_map = {
    "amd64": "AMD64",
}
