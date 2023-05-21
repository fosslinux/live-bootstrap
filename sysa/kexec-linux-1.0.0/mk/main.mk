# SPDX-FileCopyrightText: 2023 Richard Masters <grick23@gmail.com>
# SPDX-License-Identifier: MIT
kexec-linux: kexec-linux.c
	gcc -static -m32 -march=i386 -o $@ $^
