.. SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
.. SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
.. SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>

.. SPDX-License-Identifier: CC-BY-SA-4.0


live-bootstrap
==============

An attempt to provide a reproducible, automatic, complete end-to-end
bootstrap from a minimal number of binary seeds to a supported fully
functioning operating system.

Get me started!
---------------

1. ``git clone https://github.com/fosslinux/live-bootstrap``
2. ``git submodule update --init --recursive``
3. Provide a kernel (vmlinuz file) as the name kernel in the root of the
   repository. **This must be a 32-bit kernel.**
4. ``./rootfs.py --qemu`` - ensure your account has kvm privileges and qemu
   installed.

   a. Alternatively, run ``./rootfs.py --chroot`` to run it in a chroot.
   b. Alternatively, run ``./rootfs.py`` but don’t run the actual
      virtualization and instead copy sysa/tmp/initramfs to a USB or
      some other device and boot from bare metal. NOTE: we now require
      a hard drive. This is currently hardcoded as sda. You also need
      to put ``sysc/tmp/disk.img`` onto your sda on the bootstrapping
      machine.

5. Wait.
6. If you can, observe the many binaries in ``/after/bin``! When the
   bootstrap is completed ``bash`` is launched providing a shell to
   explore the system.

Background
----------

This project is a part of the bootstrappable project, a project that
aims to be able to build complete computing platforms through the use of
source code. When you build a compiler like GCC, you need another C
compiler to compile the compiler - turtles all the way down. Even the
first GCC compiler was written in C. There has to be a way to break the
chain…

There has been significant work on this over the last 5 years, from
Jeremiah Orians’ stage0, hex2 and M2-Planet to janneke’s Mes. We have a
currently, fully-functioning chain of bootstrapping from the 357-byte
hex0 seed to a complete GCC compiler and hence a full Linux operating
system. From there, it is trivial to move to other UNIXes. However,
there is only currently one vector through which this can be
automatically done, GNU Guix.

While the primary author of this project does not believe Guix is a bad
project, the great reliance on Guile, the complexity of many of the
scripts and the rather steep learning curve to install and run Guix make
it a very non plug-and-play solution. Furthermore, there is currently
(Jan 2021) no possible way to run the bootstrap from outside of a
pre-existing Linux environment. Additionally, Guix uses many scripts and
distributed files that cannot be considered source code.

(NOTE: Guix is working on a Full Source Bootstrap, but I’m not
completely sure what that entails).

Furthermore, having an alternative bootstrap automation tool allows
people to have greater trust in the bootstrap procedure.

Comparison between GNU Guix and live-bootstrap
----------------------------------------------

+----------------------+----------------------+----------------------+
| Item                 | Guix                 | live-bootstrap       |
+======================+======================+======================+
| Total size of seeds  | ~30MB (Reduced       | ~1KB                 |
| [1]                  | Source Bootstrap)    |                      |
|                      | [2]                  |                      |
+----------------------+----------------------+----------------------+
| Use of kernel        | Linux-Libre Kernel   | Any Linux Kernel     |
|                      |                      | (2.6+) [3]           |
+----------------------+----------------------+----------------------+
| Implementation       | Yes                  | No (in development)  |
| complete             |                      |                      |
+----------------------+----------------------+----------------------+
| Automation           | Almost fully         | Optional user        |
|                      | automatic            | customization        |
+----------------------+----------------------+----------------------+

[1]: Both projects only use software licensed under a FSF-approved
free software license.
[2]: Reiterating that Guix is working on a full source bootstrap,
although that still uses guile (~12 MB). [3]: Work is ongoing to use
other, smaller POSIX kernels.

Why would I want bootstrapping?
-------------------------------

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

1. Binaries (apart from seed hex0, kaem, kernel).
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

sysa
~~~~

sysa is the first ‘system’ used in live-bootstrap. We move to a new
system after a reboot, which often occurs after the movement to a new
kernel. It is run by the seed Linux kernel provided by the user. It
compiles everything we need to be able to compile our own Linux kernel.
It runs fully in an initramfs and does not rely on disk support in the
seed Linux kernel.

sysb
~~~~

sysb is the second 'system' of live-bootstrap. This uses the Linux 4.9.10
kernel compiled within sysa. As we do not rely on disk support in sysa, we
need this intermediate system to be able to add the missing binaries to sysc
before moving into it. This is executed through kexec from sysa. At this point,
a SATA disk IS required.

sysc
~~~~

sysc is the (current) last 'system' of live-bootstrap. This is a continuation
from sysb, executed through util-linux's ``switch_root`` command which moves
the entire rootfs without a reboot. Every package from here on out is compiled
under this system, taking binaries from sysa. Chroot mode skips sysb, as it
is obviously irrelevant for a chroot.
