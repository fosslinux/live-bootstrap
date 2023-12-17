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

class Generator():
    """
    Class responsible for generating the basic media to be consumed.
    """

    git_dir = os.path.join(os.path.dirname(os.path.join(__file__)), '..')
    distfiles_dir = os.path.join(git_dir, 'distfiles')

    # pylint: disable=too-many-arguments
    def __init__(self, tmpdir, arch, external_sources,
                 early_preseed, repo_path):
        self.arch = arch
        self.early_preseed = early_preseed
        self.external_sources = external_sources
        self.repo_path = repo_path
        self.tmpdir = tmpdir
        self.tmp_dir = tmpdir.path
        self.external_dir = os.path.join(self.tmp_dir, 'external')

    def prepare(self, using_kernel=False, kernel_bootstrap=False):
        """
        Prepare basic media of live-bootstrap.
        /steps -- contains steps to be built
        / -- contains seed to allow steps to be built, containing custom
             scripts and stage0-posix
        """
        # We use ext3 here; ext4 actually has a variety of extensions that
        # have been added with varying levels of recency
        # Linux 4.9.10 does not support a bunch of them
        # Attempting to disable extensions that a particular e2fsprogs
        # is *unaware* of causes the filesystem creation to fail
        # We could hypothetically detect e2fsprogs version and create an
        # argument matrix ... or we could just use ext3 instead which
        # is effectively universally the same
        if kernel_bootstrap:
            init_path = os.path.join(self.tmp_dir, 'init')

            os.mkdir(init_path)
            self.tmp_dir = init_path

            if self.repo_path or self.external_sources:
                self.tmpdir.add_disk("external", filesystem="ext3")
                self.tmpdir.mount_disk("external", "external")
        elif using_kernel:
            self.tmp_dir = os.path.join(self.tmp_dir, 'disk')
            self.tmpdir.add_disk("disk", filesystem="ext3")
            self.tmpdir.mount_disk("disk", "disk")
            self.external_dir = os.path.join(self.tmp_dir, 'external')

        os.makedirs(self.external_dir, exist_ok=True)

        if self.early_preseed:
            # Extract tar containing preseed
            with tarfile.open(self.early_preseed, "r") as seed:
                seed.extractall(self.tmp_dir)
            shutil.copy2(os.path.join(self.git_dir, 'seed', 'preseeded.kaem'),
                         os.path.join(self.tmp_dir, 'kaem.x86'))
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
            self.create_builder_hex0_disk_image(os.path.join(self.tmp_dir, 'disk.img'))

        if kernel_bootstrap and (self.external_sources or self.repo_path):
            self.tmpdir.umount_disk('external')
        elif using_kernel:
            self.tmpdir.umount_disk('disk')

    def steps(self):
        """Copy in steps."""
        source_manifest = self.get_source_manifest()
        self.get_packages(source_manifest)

        shutil.copytree(os.path.join(self.git_dir, 'steps'), os.path.join(self.tmp_dir, 'steps'))

    def stage0_posix(self):
        """Copy in all of the stage0-posix"""
        stage0_posix_base_dir = os.path.join(self.git_dir, 'seed', 'stage0-posix')
        for f in os.listdir(stage0_posix_base_dir):
            orig = os.path.join(stage0_posix_base_dir, f)
            to = os.path.join(self.tmp_dir, f)
            if os.path.isfile(orig):
                shutil.copy2(orig, to)
            else:
                shutil.copytree(orig, to)

        arch = stage0_arch_map.get(self.arch, self.arch)
        kaem_optional_seed = os.path.join(self.git_dir, 'seed', 'stage0-posix', 'bootstrap-seeds',
                                          'POSIX', arch, 'kaem-optional-seed')
        shutil.copy2(kaem_optional_seed, os.path.join(self.tmp_dir, 'init'))

    def seed(self):
        """Copy in extra seed files"""
        seed_dir = os.path.join(self.git_dir, 'seed')
        for f in os.listdir(seed_dir):
            if os.path.isfile(os.path.join(seed_dir, f)):
                shutil.copy2(os.path.join(seed_dir, f), os.path.join(self.tmp_dir, f))

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
        file_list_path = os.path.join(self.tmp_dir, 'steps', 'lwext4-1.0.0-lb1',
                                      'files', 'fiwix-file-list.txt')
        shutil.copyfile(os.path.join(self.tmp_dir, 'steps', 'lwext4-1.0.0-lb1',
                                     'files', 'early-artifacts-needed-after-fiwix.txt'),
                        file_list_path)

        save_cwd = os.getcwd()
        os.chdir(self.tmp_dir)
        self.add_fiwix_files(file_list_path, 'steps')
        self.add_fiwix_files(file_list_path, 'distfiles')
        os.chdir(save_cwd)

    def distfiles(self):
        """Copy in distfiles"""
        def copy_no_network_distfiles(out):
            # Note that no network == no disk for kernel bootstrap mode
            pre_src_path = os.path.join(self.git_dir, 'steps', 'pre-network-sources')
            with open(pre_src_path, 'r', encoding="utf-8") as source_list:
                for file in source_list.readlines():
                    file = file.strip()
                    shutil.copy2(os.path.join(self.distfiles_dir, file),
                                 os.path.join(out, file))

        early_distfile_dir = os.path.join(self.tmp_dir, 'external', 'distfiles')
        main_distfile_dir = os.path.join(self.external_dir, 'distfiles')

        if early_distfile_dir != main_distfile_dir:
            os.makedirs(early_distfile_dir)
            copy_no_network_distfiles(early_distfile_dir)

        if self.external_sources:
            os.mkdir(main_distfile_dir)
            shutil.copytree(self.distfiles_dir, main_distfile_dir)
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

        os.chdir(self.tmp_dir)
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

    def create_builder_hex0_disk_image(self, image_file_name):
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

        # fill file with zeros up to desired size, one megabyte at a time
        with open(image_file_name, 'ab') as image_file:
            while current_size < 16384 * megabyte:
                image_file.write(b'\0' * megabyte)
                current_size += megabyte

    def check_file(self, file_name, expected_hash):
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

    def download_file(self, url, directory, file_name):
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
                raise requests.HTTPError("Download failed.")
        return abs_file_name

    def get_packages(self, source_manifest):
        """Prepare remaining sources"""
        for line in source_manifest.split("\n"):
            line = line.strip().split(" ")

            path = self.download_file(line[2], line[1], line[3])
            self.check_file(path, line[0])

    @classmethod
    def get_source_manifest(cls):
        """
        Generate a source manifest for the system.
        """
        manifest_lines = []
        directory = os.path.relpath(cls.distfiles_dir, cls.git_dir)

        # Find all source files
        steps_dir = os.path.join(cls.git_dir, 'steps')
        for file in os.listdir(steps_dir):
            if os.path.isdir(os.path.join(steps_dir, file)):
                sourcef = os.path.join(steps_dir, file, "sources")
                if os.path.exists(sourcef):
                    # Read sources from the source file
                    with open(sourcef, "r", encoding="utf_8") as sources:
                        for line in sources.readlines():
                            line = line.strip().split(" ")

                            if len(line) > 2:
                                file_name = line[2]
                            else:
                                # Automatically determine file name based on URL.
                                file_name = os.path.basename(line[0])

                            manifest_lines.append(f"{line[1]} {directory} {line[0]} {file_name}")

        return "\n".join(manifest_lines)

stage0_arch_map = {
    "amd64": "AMD64",
}
