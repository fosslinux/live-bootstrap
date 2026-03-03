#!/usr/bin/env python3
"""
This file contains all code required to generate the boot image for live-bootstrap
"""
# SPDX-License-Identifier: GPL-3.0-or-later
# SPDX-FileCopyrightText: 2022-2023 Dor Askayo <dor.askayo@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2021-23 Samuel Tyler <samuel@samuelt.me>

import hashlib
import os
import random
import shutil
import struct
import tarfile
import traceback

import requests

# pylint: disable=too-many-instance-attributes
class Generator():
    """
    Class responsible for generating the basic media to be consumed.
    """

    git_dir = os.path.join(os.path.dirname(os.path.join(__file__)), '..')
    distfiles_dir = os.path.join(git_dir, 'distfiles')
    raw_container_magic = b'LBPAYLD1'

    # pylint: disable=too-many-arguments,too-many-positional-arguments
    def __init__(self, arch, external_sources, early_preseed, repo_path, mirrors,
                 build_guix_also=False):
        self.arch = arch
        self.early_preseed = early_preseed
        self.external_sources = external_sources
        self.repo_path = repo_path
        self.mirrors = mirrors
        self.build_guix_also = build_guix_also
        self.source_manifest = self.get_source_manifest(
            stop_before_improve=("get_network" if not self.external_sources else None),
            build_guix_also=self.build_guix_also
        )
        self.early_source_manifest = self.get_source_manifest(
            stop_before_improve="get_network",
            build_guix_also=self.build_guix_also
        )
        self.bootstrap_source_manifest = self.source_manifest
        self.external_source_manifest = []
        self.external_image = None
        self.kernel_bootstrap_mode = None
        self.target_dir = None
        self.external_dir = None

    def reuse(self, target):
        """
        Reuse a previously prepared bwrap environment for further stages.
        """
        self.target_dir = target.path
        self.external_dir = os.path.join(self.target_dir, 'external')
        self.distfiles()

    def _select_kernel_bootstrap_mode(self):
        """
        Select how kernel-bootstrap should transport distfiles.
        """
        if self.repo_path:
            self.kernel_bootstrap_mode = "repo"
            self.external_source_manifest = []
            return

        if self.external_sources:
            self.kernel_bootstrap_mode = "raw_external"
            self._prepare_kernel_bootstrap_external_manifests()
            return

        self.kernel_bootstrap_mode = "network_only"
        self.bootstrap_source_manifest = self.early_source_manifest
        self.external_source_manifest = []

    def _prepare_kernel_bootstrap_external_manifests(self):
        """
        Split distfiles between init image and external raw container.
        """
        # Keep the early builder image small: include only sources needed
        # before improve: import_payload runs, so external.img is the primary
        # carrier for the remaining distfiles.
        self.bootstrap_source_manifest = self.get_source_manifest(
            stop_before_improve="import_payload",
            build_guix_also=False
        )

        full_manifest = self.get_source_manifest(build_guix_also=self.build_guix_also)
        if self.bootstrap_source_manifest == full_manifest:
            raise ValueError("steps/manifest must include `improve: import_payload` in kernel-bootstrap mode.")
        bootstrap_set = set(self.bootstrap_source_manifest)
        self.external_source_manifest = [entry for entry in full_manifest if entry not in bootstrap_set]

    def _copy_manifest_distfiles(self, out_dir, manifest):
        os.makedirs(out_dir, exist_ok=True)
        for entry in manifest:
            file_name = entry[3].strip()
            shutil.copy2(os.path.join(self.distfiles_dir, file_name),
                         os.path.join(out_dir, file_name))

    def _ensure_manifest_distfiles(self, manifest):
        for entry in manifest:
            checksum, directory, url, file_name = entry
            distfile_path = os.path.join(directory, file_name)
            if not os.path.isfile(distfile_path):
                self.download_file(url, directory, file_name)
            self.check_file(distfile_path, checksum)

    def _create_raw_container_image(self, target_path, manifest, image_name="external.img"):
        if manifest is None:
            manifest = []

        if manifest:
            # Guarantee all payload distfiles exist and match checksums.
            self._ensure_manifest_distfiles(manifest)

        files_by_name = {}
        for checksum, _, _, file_name in manifest:
            if file_name in files_by_name and files_by_name[file_name] != checksum:
                raise ValueError(
                    f"Conflicting container file with same name but different hash: {file_name}"
                )
            files_by_name[file_name] = checksum

        container_path = os.path.join(target_path, image_name)
        ordered_names = sorted(files_by_name.keys())
        with open(container_path, "wb") as container:
            container.write(self.raw_container_magic)
            container.write(struct.pack("<I", len(ordered_names)))
            for file_name in ordered_names:
                file_name_bytes = file_name.encode("utf_8")
                if len(file_name_bytes) > 0xFFFFFFFF:
                    raise ValueError(f"Container file name too long: {file_name}")

                src_path = os.path.join(self.distfiles_dir, file_name)
                file_size = os.path.getsize(src_path)
                if file_size > 0xFFFFFFFF:
                    raise ValueError(f"Container file too large for raw container format: {file_name}")

                container.write(struct.pack("<II", len(file_name_bytes), file_size))
                container.write(file_name_bytes)

                with open(src_path, "rb") as src_file:
                    shutil.copyfileobj(src_file, container, 1024 * 1024)

        return container_path

    def prepare(self, target, using_kernel=False, kernel_bootstrap=False, target_size=0):
        """
        Prepare basic media of live-bootstrap.
        /steps -- contains steps to be built
        / -- contains seed to allow steps to be built, containing custom
             scripts and stage0-posix
        """
        self.target_dir = target.path
        self.external_dir = os.path.join(self.target_dir, 'external')
        self.external_image = None
        self.external_source_manifest = []
        self.bootstrap_source_manifest = self.source_manifest
        self.kernel_bootstrap_mode = None

        # We use ext3 here; ext4 actually has a variety of extensions that
        # have been added with varying levels of recency
        # Linux 4.9.10 does not support a bunch of them
        # Attempting to disable extensions that a particular e2fsprogs
        # is *unaware* of causes the filesystem creation to fail
        # We could hypothetically detect e2fsprogs version and create an
        # argument matrix ... or we could just use ext3 instead which
        # is effectively universally the same
        if kernel_bootstrap:
            self.target_dir = os.path.join(self.target_dir, 'init')
            os.mkdir(self.target_dir)
            self._select_kernel_bootstrap_mode()
        elif using_kernel:
            self.target_dir = os.path.join(self.target_dir, 'disk')
            self.external_dir = os.path.join(self.target_dir, 'external')

        if self.early_preseed:
            # Extract tar containing preseed
            with tarfile.open(self.early_preseed, "r") as seed:
                seed.extractall(self.target_dir)
            if os.path.exists(os.path.join(self.target_dir, 'steps')):
                shutil.rmtree(os.path.join(self.target_dir, 'steps'))
            if os.path.exists(self.external_dir):
                shutil.rmtree(self.external_dir)
            shutil.copy2(os.path.join(self.git_dir, 'seed', 'preseeded.kaem'),
                         os.path.join(self.target_dir, 'kaem.x86'))
        else:
            self.stage0_posix(kernel_bootstrap)
            self.seed()

        os.makedirs(self.external_dir)

        self.steps()

        self.distfiles()

        if self.repo_path:
            repo_dir = os.path.join(self.external_dir, 'repo-preseeded')
            shutil.copytree(self.repo_path, repo_dir)

        if kernel_bootstrap:
            self.create_builder_hex0_disk_image(self.target_dir + '.img', target_size)

            if self.kernel_bootstrap_mode == "repo":
                mkfs_args = ['-d', os.path.join(target.path, 'external')]
                target.add_disk("external", filesystem="ext3", mkfs_args=mkfs_args)
            elif self.kernel_bootstrap_mode == "raw_external":
                # external.img is a raw container, imported at improve: import_payload.
                self.external_image = self._create_raw_container_image(
                    target.path,
                    self.external_source_manifest,
                    image_name="external.img",
                )
                target.add_existing_disk("external", self.external_image)
        elif using_kernel:
            mkfs_args = ['-F', '-d', os.path.join(target.path, 'disk')]
            target.add_disk("disk",
                            filesystem="ext3",
                            size=(str(target_size) + "M") if target_size else "16G",
                            bootable=True,
                            mkfs_args=mkfs_args)

    def steps(self):
        """Copy in steps."""
        self.get_packages()

        shutil.copytree(os.path.join(self.git_dir, 'steps'), os.path.join(self.target_dir, 'steps'))
        if self.build_guix_also:
            steps_guix_dir = os.path.join(self.git_dir, 'steps-guix')
            if not os.path.isdir(steps_guix_dir):
                raise ValueError("steps-guix directory does not exist while --build-guix-also is set.")
            if not os.path.isfile(os.path.join(steps_guix_dir, 'manifest')):
                raise ValueError("steps-guix/manifest does not exist while --build-guix-also is set.")
            shutil.copytree(steps_guix_dir, os.path.join(self.target_dir, 'steps-guix'))

    def stage0_posix(self, kernel_bootstrap=False):
        """Copy in all of the stage0-posix"""
        stage0_posix_base_dir = os.path.join(self.git_dir, 'seed', 'stage0-posix')
        for entry in os.listdir(stage0_posix_base_dir):
            if kernel_bootstrap and entry == 'bootstrap-seeds':
                continue
            orig = os.path.join(stage0_posix_base_dir, entry)
            target = os.path.join(self.target_dir, entry)
            if os.path.isfile(orig):
                shutil.copy2(orig, target)
            else:
                shutil.copytree(orig, target)

        if not kernel_bootstrap:
            arch = stage0_arch_map.get(self.arch, self.arch)
            kaem_optional_seed = os.path.join(self.git_dir, 'seed', 'stage0-posix',
                                              'bootstrap-seeds', 'POSIX', arch,
                                              'kaem-optional-seed')
            shutil.copy2(kaem_optional_seed, os.path.join(self.target_dir, 'init'))

    def seed(self):
        """Copy in extra seed files"""
        seed_dir = os.path.join(self.git_dir, 'seed')
        for entry in os.listdir(seed_dir):
            if os.path.isfile(os.path.join(seed_dir, entry)):
                shutil.copy2(os.path.join(seed_dir, entry), os.path.join(self.target_dir, entry))

    def distfiles(self):
        """Copy in distfiles"""
        distfile_dir = os.path.join(self.external_dir, 'distfiles')

        if self.kernel_bootstrap_mode in ("raw_external", "repo"):
            self._copy_manifest_distfiles(distfile_dir, self.bootstrap_source_manifest)
            return

        if self.kernel_bootstrap_mode == "network_only":
            self._copy_manifest_distfiles(distfile_dir, self.early_source_manifest)
            return

        if self.external_sources:
            shutil.copytree(self.distfiles_dir, distfile_dir, dirs_exist_ok=True)
        else:
            self._copy_manifest_distfiles(distfile_dir, self.bootstrap_source_manifest)

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
        cmd = ' '.join(['src',
                        '0',
                        '/bootstrap-seeds\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['src',
                        '0',
                        '/bootstrap-seeds/POSIX\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['src',
                        '0',
                        '/bootstrap-seeds/POSIX/x86\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['hex0',
                        '/x86/hex0_x86.hex0',
                        '/bootstrap-seeds/POSIX/x86/hex0-seed\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['hex0',
                        '/x86/kaem-minimal.hex0',
                        '/bootstrap-seeds/POSIX/x86/kaem-optional-seed\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['hex0',
                        '/x86/kaem-minimal.hex0',
                        '/init\n'])
        image_file.write(cmd.encode())
        cmd = ' '.join(['/bootstrap-seeds/POSIX/x86/kaem-optional-seed', '/kaem.x86\n'])
        image_file.write(cmd.encode())
        os.chdir(save_cwd)

    def create_builder_hex0_disk_image(self, image_file_name, size):
        """Create builder-hex0 disk image"""
        with open(image_file_name, 'ab') as image_file:
            # Compile and write stage1 binary seed
            with open(os.path.join('builder-hex0', 'builder-hex0-x86-stage1.hex0'),
                      encoding="utf-8") as infile:
                for line in infile:
                    image_file.write(bytes.fromhex(line.split('#')[0].split(';')[0].strip()))
            # Append stage2 hex0 source
            with open(os.path.join('builder-hex0', 'builder-hex0-x86-stage2.hex0'),
                      encoding="utf-8") as infile:
                image_file.write(infile.read().encode())

        # Close first with statement before getting file size.
        with open(image_file_name, 'ab') as image_file:
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

    def download_file(self, url, directory, file_name, silent=False):
        """
        Download a single source archive.
        """
        abs_file_name = os.path.join(directory, file_name)

        # Create a directory for downloaded file
        if not os.path.isdir(directory):
            os.mkdir(directory)

        # Actually download the file
        headers = {
                "Accept-Encoding": "identity",
                "User-Agent": "curl/7.88.1"
        }
        if not os.path.isfile(abs_file_name):
            if not silent:
                print(f"Downloading: {file_name}")

            def do_download(source):
                response = requests.get(source, allow_redirects=True, stream=True,
                        headers=headers, timeout=20)
                if response.status_code == 200:
                    with open(abs_file_name, 'wb') as target_file:
                        target_file.write(response.raw.read())
                    return True
                print(f"Download failed from {option}: {response.status_code} {response.reason}")
                return False

            done = False
            if self.mirrors:
                options = [f"{x}/{file_name}" for x in self.mirrors]
            else:
                options = []
            random.shuffle(options)
            for option in options:
                if do_download(option):
                    done = True
                    break

            if not done:
                if url == "_" or not do_download(url):
                    raise requests.RequestException(f"Unable to download {url} from ",
                                                    "any mirror or original")

        return abs_file_name

    def get_packages(self):
        """Prepare remaining sources"""
        for line in self.source_manifest:
            try:
                path = self.download_file(line[2], line[1], line[3])
            except requests.HTTPError:
                print(traceback.format_exc())
        for line in self.source_manifest:
            path = os.path.join(line[1], line[3])
            self.check_file(path, line[0])

    @classmethod
    def get_source_manifest(cls, stop_before_improve=None, build_guix_also=False):
        """
        Generate a source manifest for the system.
        """
        entries = []
        directory = os.path.relpath(cls.distfiles_dir, cls.git_dir)

        manifests = [os.path.join(cls.git_dir, 'steps')]
        if build_guix_also:
            steps_guix_dir = os.path.join(cls.git_dir, 'steps-guix')
            if not os.path.isdir(steps_guix_dir):
                raise ValueError("steps-guix directory does not exist while --build-guix-also is set.")
            manifests.append(steps_guix_dir)

        for steps_dir in manifests:
            manifest_path = os.path.join(steps_dir, 'manifest')
            if not os.path.isfile(manifest_path):
                if steps_dir.endswith('steps-guix'):
                    raise ValueError("steps-guix/manifest does not exist while --build-guix-also is set.")
                raise ValueError(f"Missing manifest: {manifest_path}")

            with open(manifest_path, 'r', encoding="utf_8") as file:
                for line in file:
                    stripped = line.strip()
                    if stop_before_improve and stripped.startswith("improve: "):
                        improve_step = stripped.split(" ")[1].split("#")[0].strip()
                        if improve_step == stop_before_improve:
                            break

                    if not stripped.startswith("build: "):
                        continue

                    step = line.split(" ")[1].split("#")[0].strip()
                    sourcef = os.path.join(steps_dir, step, "sources")
                    if os.path.exists(sourcef):
                        # Read sources from the source file
                        with open(sourcef, "r", encoding="utf_8") as sources:
                            for source in sources.readlines():
                                source = source.strip().split(" ")

                                if source[0] == "g" or source[0] == "git":
                                    source[1:] = source[2:]

                                if len(source) > 3:
                                    file_name = source[3]
                                else:
                                    # Automatically determine file name based on URL.
                                    file_name = os.path.basename(source[1])

                                entry = (source[2], directory, source[1], file_name)
                                if entry not in entries:
                                    entries.append(entry)

        return entries

stage0_arch_map = {
    "amd64": "AMD64",
}
