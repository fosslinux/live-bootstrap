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
│   ├── patches
│   └── src
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

In this folder, there are other folders/files. `src` and `checksums` are
required, others are optional.

Permissable folders/files:

- `files`: auxiliary files required for the build distributed by live-bootstrap.
- `mk`: makefiles.
- `patches`: patches for the source.
- `src`: the upstream unmodified source code. This may be either a submodule or
  nonexistent.
- `checksums`: the checksums for the resulting binaries and libraries that
  are compiled and installed. This may be either a folder or a file. It should
  be a folder when there are multiple checksumming files required (normally
  multiple seperate passes) but a file when there is only one checksumming
  file.
  - Up to and including `patch`, `fletcher16` is used for the checksumming.
  - After `patch`, `sha-2` is built which contains an external implementation of
    `sha256sum`. We then use that currently for all remaining software.
  - To extract the binaries to get their checksums, use of chroot mode is
    recommended (i.e. `./rootfs.sh chroot`).

Furthermore, there is a special top-level dir `dev-utils`. In here are
appropriate utilities that are often useful for development and not generally
included on normal systems, or are specific to live-bootstrap. Each program
should be contained within its own directory and include:

- source code
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
