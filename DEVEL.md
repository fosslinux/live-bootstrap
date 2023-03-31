<!--
SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Development Guidelines

## Testing

Before making a PR, please test your change locally. It is OK to develop outside
of the live environment, but please be sure to test inside the live environment
and that a full build completes.

## Structure

Each system corresponds to a reboot of the live environment. There is only one
appropriate structure as shown below (eg for sysa):

```
sysa
├── any-global-files.sh
├── somepackage-version
│   ├── somepackage-version.kaem (or .sh)
│   ├── files
│   ├── mk
│   └── patches
└── tmp
```

Global scripts that drive the entire system go directly under `sysx`. `tmp`
contains the temporary system used for QEMU or a chroot.

Then, each package is in its own specific directory, named `package-version`.
It then diverges based upon which driver is being used:

- `kaem`: A file named `package-version.kaem` is called by the master script.
- `bash`: The `build` function from helper.sh is called from the master script.
  There are default functions run which can be overridden by an optional script
  `package-version.sh` within the package-specific directory.

In this folder, there are other folders/files. `*.checksums` are
required for early packages that are build with kaem, others are optional.

Permissible folders/files:

- `files`: auxiliary files required for the build distributed by live-bootstrap.
- `mk`: makefiles.
- `patches`: patches for the source.
- `simple-patches`: patches for the source that use the before/after convention of simple-patch.c
- `*.checksums`: files containing the checksums for the resulting binaries and
libraries that are compiled and installed.
  - Up to and including `coreutils-6.10`, `sha256sum` from `stage0-posix`
    is used for the checksumming. Later we switch to GNU version.
  - To extract checksums of the binaries, use of qemu mode is recommended
    (i.e. `./rootfs.py -q -qk $kernel --update-checksums`).
- compilation script

The directory m2-functions is used for M2-Planet functions (currently).

## Conventions

- **Patches:**
  - all patches are `-p0`
  - all patches begin with a patch header
- **README:**
  - all stages are explained in README
- **General:**
  - Where possible, all blocks of text should be limited to a length of 80
    characters.
  - There is no character limit for code, the reasons for this are two-fold:
    - Often harms readability.
    - Often impossible/hard in early bootstrap stages.
- **Licensing:**
  - Generally, the project is licensed under the GNU GPL v3.0 (or any later
    version).
  - Documentation is licensed under CC-BY-SA-4.0.
  - Patches are licensed under the license of the project which they are
    patching.
  - All files (excluding files within submodules) must comply with REUSE v3.0.

## git

All changes must be submitted as PRs. Pushing to master is disallowed, even if
push access is granted to a user. Only pushes to master should be merging of
patches into master.
