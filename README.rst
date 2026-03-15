.. SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
.. SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
.. SPDX-FileCopyrightText: 2021 Samuel Tyler <samuel@samuelt.me>

.. SPDX-License-Identifier: CC-BY-SA-4.0


live-bootstrap
==============

An attempt to provide a reproducible, automatic, complete end-to-end
bootstrap from a minimal number of binary seeds to a supported fully
functioning operating system.

How do I use this?
------------------

Quick start:

Choose a mirror from https://github.com/fosslinux/live-bootstrap/wiki/Mirrors,
or create a private/public mirror yourself (see further below). You should
provide the mirror as ``--mirror`` to ``rootfs.py``.

See ``./rootfs.py --help`` and follow the instructions given there.
This uses a variety of userland tools to prepare the bootstrap.

(*Currently, there is no way to perform the bootstrap without external
preparations! This is a currently unsolved problem.*)

Without using Python:

0. Choose a mirror as detailed above. (You will input this later, instead of
   passing it to ``rootfs.py```).
1. ``git clone https://github.com/fosslinux/live-bootstrap``
2. ``git submodule update --init --recursive``
3. Consider whether you are going to run this in a chroot, in QEMU, or on bare
   metal. (All of this *can* be automated, but not in a trustable way. See
   further below.)

   a. **chroot:** Create a directory where the chroot will reside, run
      ``./download-distfiles.sh``, and copy:

      * The entire contents of ``seed/stage0-posix`` into that directory.
      * All other files in ``seed`` into that directory.
      * ``steps/`` and ``distfiles/`` into that directory.

        * At least all files listed in ``steps/pre-network-sources`` must be
          copied in. All other files will be obtained from the network.
      * Run ``/bootstrap-seeds/POSIX/x86/kaem-optional-seed`` in the chroot.
        (Eg, ``chroot rootfs /bootstrap-seeds/POSIX/x86/kaem-optional-seed``).
   b. **QEMU:** Create two blank disk images.

      * Generate ``builder-hex0-x86-stage1.img`` from hex0 source:

        ``sed 's/[;#].*$//g' builder-hex0/builder-hex0-x86-stage1-hex0 | xxd -r -p``
      * On the first image, write ``builder-hex0-x86-stage1.img`` to it, followed
        by ``kernel-bootstrap/builder-hex0-x86-stage2.hex0``, followed by zeros
        padding the disk to the next sector.
      * distfiles can be obtained using ``./download-distfiles.sh``.
      * See the list in part a. For every file within that list, write a line to
        the disk ``src <size-of-file> <path-to-file>``, followed by the contents
        of the file.

        * *Only* copy distfiles listed in ``sources`` files for ``build:`` steps
          manifested before ``improve: get_network`` into this disk.
      * In kernel-bootstrap mode with ``--external-sources`` (and no ``--repo``),
        use the second image as ``external.img``.
        ``external.img`` is a raw container (not a filesystem) used to carry the
        distfiles that are not needed before ``improve: import_payload``.
        In other words, the first image only carries the minimal set needed to
        reach the importer; the rest of the distfiles live in ``external.img``.

        * Header magic: ``LBPAYLD1`` (8 bytes).
        * Then: little-endian ``u64`` file count.
        * Repeated for each file: little-endian ``u64`` name length,
          little-endian ``u64`` file size, UTF-8 encoded file name bytes
          (no terminator), raw file bytes.
        * ``name length`` is the number of UTF-8 bytes (not Unicode code points).

      * With ``--repo``, the second disk remains an ext3 distfiles/repo disk.
      * Without ``--external-sources`` and without ``--repo``, no second disk is
        used: the initial image includes only pre-network distfiles, and later
        distfiles are downloaded from configured mirrors after networking starts.
      * Run QEMU, with 4+G RAM, optionally SMP (multicore), both drives (main
        builder image plus external image, when a second image is used), a NIC with model E1000
        (``-nic user,model=e1000``), and ``-machine kernel-irqchip=split``.
   c. **Bare metal:** Follow the same steps as QEMU, but the disks need to be
      two different *physical* disks, and boot from the first disk.

Manual raw ``external.img`` preparation
---------------------------------------

The following script creates a raw ``external.img`` from a manually prepared
file list. This is equivalent to what ``rootfs.py`` does for kernel-bootstrap
with ``--external-sources`` (and no ``--repo``).

1. Prepare an ``external.list`` with one file per line, formatted as:
   ``<archive-name> <absolute-path-to-archive>``.
2. Run:

   ::

      cat > make-payload.sh <<'EOF'
      #!/bin/sh
      set -e
      out="${1:-external.img}"
      list="${2:-external.list}"

      write_u64le() {
          v="$1"
          printf '%016x' "$v" | sed -E 's/(..)(..)(..)(..)(..)(..)(..)(..)/\8\7\6\5\4\3\2\1/' | xxd -r -p
      }

      count="$(wc -l < "${list}" | tr -d ' ')"
      : > "${out}"
      printf 'LBPAYLD1' >> "${out}"
      write_u64le "${count}" >> "${out}"

      while read -r name path; do
          [ -n "${name}" ] || continue
          size="$(wc -c < "${path}" | tr -d ' ')"
          name_len="$(printf '%s' "${name}" | wc -c | tr -d ' ')"
          write_u64le "${name_len}" >> "${out}"
          write_u64le "${size}" >> "${out}"
          printf '%s' "${name}" >> "${out}"
          cat "${path}" >> "${out}"
      done < "${list}"
      EOF
      chmod +x make-payload.sh
      ./make-payload.sh external.img external.list

3. Attach ``external.img`` as an additional raw disk when booting in QEMU, or
   as the second physical disk on bare metal.

Notes:

* ``external.img`` raw container mode is used with ``--external-sources`` (and
  no ``--repo``). With ``--build-guix-also``, the container content is larger
  because it also includes post-early sources from ``steps-guix``.
* Without ``--external-sources`` and without ``--repo``, there is no second
  image. The initial image only includes distfiles needed before
  ``improve: get_network``; later distfiles are downloaded from mirrors.
* The runtime importer identifies the correct disk by checking the magic
  ``LBPAYLD1`` on each detected block device, not by assuming a device name.

Mirrors
-------

It has been decided that repackaging distfiles for live-bootstrap is generally
permissible, particularly from git repositories. We do this primarily because

a. currently live-bootstrap only supports tarballs/raw files as input, not git
   repositories
b. to reduce load on servers

You may choose to use an existing mirror from 
https://github.com/fosslinux/live-bootstrap/wiki/Mirrors, however you may be
(to some varied extent) trusting the operator of the mirror.

Alternatively, you can create your own local mirror - one such implementation
is in ``./mirror.sh``. You can invoke it with
``./mirror.sh path/to/mirror/dir path/to/mirror/state``.
You would then pass ``--mirror path/to/mirror/dir`` to rootfs.py.
(If not using rootfs.py, you need to copy files around manually into distfiles.)

Most helpfully to the project, you could create your own public mirror, by
running ``./mirror.sh`` or writing your own script that does something similar
on a timer (systemd timer or cron job, for example), where the mirror directory
is publicly accessible on the Internet (ideally, via HTTP and HTTPS).

Background
----------

Problem statement
=================

live-bootstrap's overarching problem statement is;

> How can a usable Linux system be created with only human-auditable, and
wherever possible, human-written, source code?

Clarifications:

* "usable" means a modern toolchain, with appropriate utilities, that can be
  used to expand the amount of software on the system, interactively, or
  non-interactively.
* "human-auditable" is discretionary, but is usually fairly strict. See
  "Specific things to be bootstrapped" below.

Why is this difficult?
======================

The core of a modern Linux system is primarily written in C and C++. C and C++
are **self-hosting**, ie, nearly every single C compiler is written in C.

Every single version of GCC was written in C. To avoid using an existing
toolchain, we need some way to be able to compile a GCC version without C. We
can use a less well-featured compiler, TCC, to do this. And so forth, until we
get to a fairly primitive C compiler written in assembly, ``cc_x86``.

Going up through this process requires a bunch of other utilities as well; the
autotools suite, guile and autogen, etc. These also have to be matched
appropriately to the toolchain available.

Why should I care?
------------------

That is outside of the scope of this README. Here’s a few things you can
look at:

-  https://bootstrappable.org
-  Trusting Trust Attack (as described by Ken Thompson)
-  https://guix.gnu.org/manual/en/html_node/Bootstrapping.html
-  Collapse of the Internet (eg CollapseOS)

Specific things to be bootstrapped
----------------------------------

GNU Guix is currently the furthest along project to automate
bootstrapping. However, there are a number of non-auditable files used
in many of their packages. Here is a list of file types that we deem
unsuitable for bootstrapping.

1. Binaries (apart from seed hex0, kaem, builder-hex0).
2. Any pre-generated configure scripts, or Makefile.in’s from autotools.
3. Pre-generated bison/flex parsers (identifiable through a ``.y``
   file).
4. Any source code/binaries downloaded within a software’s build system
   that is outside of our control to verify before use in the build
   system.
5. Any non-free software. (Must be FSF-approved license).

How does this work?
-------------------

**For a more in-depth discussion, see parts.rst.**

Firstly, ``builder-hex0`` is launched. ``builder-hex0`` is a minimal kernel that is
written in ``hex0``, existing in 3 self-bootstrapping stages.

This is capable of executing the entirety of ``stage0-posix``, (see
``seed/stage0-posix``), which produces a variety of useful utilities and a basic
C language, ``M2-Planet``.

``stage0-posix`` runs a file called ``after.kaem``. This is a shell script that
builds and runs a small program called ``script-generator``. This program reads
``steps/manifest`` and converts it into a series of shell scripts that can be
executed in sequence to complete the bootstrap.

From this point forward, ``steps/manifest`` is effectively self documenting.
Each package built exists in ``steps/<pkg>``, and the build scripts can be seen
there.
