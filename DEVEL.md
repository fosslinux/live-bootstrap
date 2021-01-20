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
Within that, there must be a driving script, labelled either
`package-version.kaem` when driven by kaem or `package-version.sh` when driven
by a shell (normally bash).

In this folder, there are other folders. `src` is required, others are optional.
Permissable folders:

- `files`: auxiliary files required for the build distributed by live-bootstrap.
- `mk`: makefiles.
- `patches`: patches for the source.
- `src`: the upstream unmodified source code. This must be either:
  - a submodule
  - a folder contianing only `.placeholder` as distributed by git and gitignored,
    where the tarball is saved to in rootfs.sh. (The gitignore is already
    global, so the first time it is created with `.placeholder` it must be git
    added with `-f`).
