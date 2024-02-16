/* SPDX-FileCopyrightText: 2024 Richard Masters <grick23@gmail.com> */
/* SPDX-License-Identifier: MIT */
#undef NR_PROCS
#define NR_PROCS	4096
#undef NR_CALLOUTS
#define NR_CALLOUTS	NR_PROCS
#undef NR_OPENS
#define NR_OPENS	4096
#undef NR_FLOCKS
#define NR_FLOCKS	(NR_PROCS * 5)
#undef MAX_PID_VALUE
#define MAX_PID_VALUE	16000000
#undef RAMDISK_DRIVES
#define RAMDISK_DRIVES	0
#define CONFIG_SYSCALL_6TH_ARG
#define CONFIG_VM_SPLIT22
#define CONFIG_KEXEC
#undef CONFIG_OFFSET64
#define CONFIG_MMAP2
#define CONFIG_64BIT_SYSCALLS
#undef CONFIG_NET
#undef CONFIG_PRINTK64
