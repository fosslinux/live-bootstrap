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

```
seed
├── seed.kaem
├── script-generator.c
├── ...
└── stage0-posix

steps
├── manifest
├── any-global-files
├── jump
│   └── linux.sh
├── improve
│   └── x.sh
├── somepackage-version
│   ├── pass1.kaem
│   ├── pass2.sh
│   ├── files
│   ├── simple-patches
│   ├── mk
│   └── patches
```

The `seed` directory contains everything required for `script-generator` to be
run.

In the `steps` directory, the bootstrap process is defined in `manifest`.
Each package to be built is named `package-version`.
Each subsequent build of a package is the nth pass. Scripts are named
accordingly; eg, the first build would be called `pass1.sh`, the second would be
`pass2.sh`, etc.
Scripts run in kaem era should be denoted as such in their filename;
`pass1.kaem`, for example. Pass numbers do not reset after kaem, ie, you cannot
have both `pass1.kaem` and `pass1.sh`.

In this folder, there are other folders/files. `*.checksums` are
required for early packages that are build with kaem, others are optional.

Permissible folders/files:

- `files`: auxiliary files required for the build distributed by live-bootstrap.
- `mk`: makefiles.
- `patches`: patches for the source.
- `simple-patches`: patches for the source that use the before/after convention of simple-patch.c
- `*.checksums`: files containing the checksums for the resulting binaries and
libraries that are compiled and installed.
  - Otherwise, the package's checksum is in SHA256SUMS.pkgs.
- compilation script(s)

## Conventions

- **Patches:**
  - all patches are relative to the directory where the tarball is extracted
    - thus, no patch lines for `a/file.c`, but `coreutils-9.4/file.c`
  - all patches begin with a patch header
- **parts.rst:**
  - all packages are explained in `parts.rst`
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
