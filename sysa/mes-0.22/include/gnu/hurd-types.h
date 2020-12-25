/* C declarations for Hurd server interfaces
   Copyright (C) 1993, 1994, 1995, 1996, 1998, 1999, 2001, 2002,
   2010 Free Software Foundation, Inc.

This file is part of the GNU Hurd.

The GNU Hurd is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The GNU Hurd is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU Hurd; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef _HURD_TYPES_H
#define _HURD_TYPES_H

#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS 32
#endif

#include <time.h>		/* For struct timespec.  */
#include <mach/std_types.h>	/* For mach_port_t et al. */
#include <mach/message.h>	/* For mach_msg_id_t et al. */
#include <sys/types.h>		/* For pid_t and uid_t.  */

/* A string identifying this release of the GNU Hurd.  Our
   interpretation of the term "release" is that it refers to a set of
   server interface definitions.  A "version" in Posix terminology is
   a distribution of the Hurd; there may be more than one distribution
   without changing the release number.  */
#define HURD_RELEASE "0.0"


/*   Simple type declarations   */

/* These types identify certain kinds of ports used by the Hurd. */
typedef mach_port_t file_t;
typedef mach_port_t fsys_t;
typedef mach_port_t io_t;
typedef mach_port_t process_t;
typedef mach_port_t auth_t;
typedef mach_port_t socket_t;
typedef mach_port_t pf_t;	/* Protocol family */
typedef mach_port_t addr_port_t;
typedef mach_port_t startup_t;
typedef mach_port_t fs_notify_t;
typedef mach_port_t exec_startup_t;
typedef mach_port_t interrupt_t;
typedef mach_port_t proccoll_t;
typedef mach_port_t ctty_t;

#include <errno.h>		/* Defines `error_t'.  */

/* These names exist only because of MiG deficiencies.
   You should not use them in C source; use the normal C types instead.  */
typedef char *data_t;
typedef const char *const_data_t;
typedef char string_t [1024];
typedef int *intarray_t;
typedef const int *const_intarray_t;
typedef int *fd_mask_t;
typedef const int *const_fd_mask_t;
typedef mach_port_t *portarray_t;
typedef const mach_port_t *const_portarray_t;
typedef pid_t *pidarray_t;
typedef const pid_t *const_pidarray_t;
typedef uid_t *idarray_t;
typedef const uid_t *const_idarray_t;
#if 0
typedef __loff_t *off_array_t;
typedef const __loff_t *const_off_array_t;
#endif
typedef struct rusage rusage_t;
typedef struct flock flock_t;
typedef struct utsname utsname_t;
#if _FILE_OFFSET_BITS == 64
typedef struct stat io_statbuf_t;
typedef struct statfs fsys_statfsbuf_t;
#else
typedef struct stat64 io_statbuf_t;
typedef struct statfs64 fsys_statfsbuf_t;
#endif
typedef struct timespec timespec_t;


/*   Parameters and flags in RPC calls   */

/* Many such parameters and flags are also defined in various libc
   headers. */

/* Bits for flags in fs.defs:file_exec_paths and exec.defs:exec_* calls: */
#define EXEC_NEWTASK	0x00000001 /* Create new task; kill old one.  */
#define EXEC_SECURE	0x00000002 /* Use secure values of portarray, etc. */
#define EXEC_DEFAULTS	0x00000004 /* Use defaults for unspecified ports.  */
#define EXEC_SIGTRAP	0x00000008 /* Simulate SIGTRAP on startup.  */
/* This flag is passed through by the exec server but not examined by it.  */
#define	EXEC_STACK_ARGS	0x00000010 /* Use arguments from stack, not RPC.  */

/* Bits for flags in fs.defs:file_set_translator call: */
#define FS_TRANS_FORCE     0x00000001 /* Must use translator(no sht circuit) */
#define FS_TRANS_EXCL      0x00000002 /* Don't do it if already translated.  */
#define FS_TRANS_SET	   0x00000004 /* Set or clear translator */
#define FS_TRANS_ORPHAN    0x00000008 /* Orphan the active translator.  */

/* Values for retry field in fs.defs:dir_lookup call: */
enum retry_type
{
  FS_RETRY_NORMAL = 1,		/* Retry normally if retry_name is not null. */
  FS_RETRY_REAUTH = 2,		/* Retry after reauthenticating retry port.
				   Even if the retry name is null, a retry
				   is still necessary with this code after the
				   reauthentication is complete. */
  FS_RETRY_MAGICAL = 3,		/* Retry string is magical.  */
  /* "tty" means controlling tty;

     "fd/%u" means file descriptor N;

     "machtype/..." means replace `machtype' with the numbers in decimal
		    returned by the user's kernel as the cpu_type (N) and
		    cpu_subtype (M) (producing N/M/...) and then retry
		    as for FS_RETRY_NORMAL.

     "/..." means retry "...", but starting from the users root directory.

     "pid/..." means replace `pid' with the PID of the current process in %u
               format and then retry as for FS_RETRY_NORMAL.
     */
};
typedef enum retry_type retry_type;

/* Types for fs_notify.defs:dir_changed call: */
enum dir_changed_type
{
  DIR_CHANGED_NULL,		/* Always sent first for sync.  */
  DIR_CHANGED_NEW,		/* Specified name has been added.  */
  DIR_CHANGED_UNLINK,		/* Specified name has been removed.  */
  DIR_CHANGED_RENUMBER,		/* Name has been the target of rename.  */
};
typedef enum dir_changed_type dir_changed_type_t;

/* Types for fs_notify.defs:file_changed call: */
enum file_changed_type
{
  FILE_CHANGED_NULL,		/* Always sent first for sync.  */
  FILE_CHANGED_WRITE,		/* File data has been written.  */
  FILE_CHANGED_EXTEND,		/* File has grown.  */
  FILE_CHANGED_TRUNCATE,	/* File has been truncated.  */
  FILE_CHANGED_META,		/* Stat information has changed, and none
				   of the previous three apply.  Not sent
				   for changes in node times.  */
};
typedef enum file_changed_type file_changed_type_t;

/* Select types for io.defs:io_select call: */
#define SELECT_READ  0x00000001
#define SELECT_WRITE 0x00000002
#define SELECT_URG   0x00000004

/* Flags for fsys.defs:fsys_goaway.  Also, these flags are sent as the
   oldtrans_flags in fs.defs:file_set_translator to describe how to
   terminate the old translator. */
#define FSYS_GOAWAY_NOWAIT    0x00000001 /* Return immediately.  */
#define FSYS_GOAWAY_NOSYNC    0x00000002 /* Don't update physical media.  */
#define FSYS_GOAWAY_FORCE     0x00000004 /* Go away despite current users.  */
#define FSYS_GOAWAY_UNLINK    0x00000008 /* Go away only if non-directory.  */
#define FSYS_GOAWAY_RECURSE   0x00000010 /* Shutdown children too.  */

/* Types of ports the terminal driver can run on top of;
   used in term.defs:term_get_bottom_type.  */
enum term_bottom_type
{
 TERM_ON_MACHDEV,
 TERM_ON_HURDIO,
 TERM_ON_MASTERPTY,
};

/* Types of storage, as returned by file_get_storage_info.

   STORAGE_DEVICE is a mach device_t (for random access devices)
   STORAGE_HURD_FILE is a hurd file_t (as if a file were mapped)
   STORAGE_TASK is a task_t (the storage is in the vm of the task)
   STORAGE_MEMORY is a memory object port
   STORAGE_ZERO is a fixed-size constant source of zeros
   STORAGE_INTERLEAVE is a set of other storage types interleaved at a fixed
    interval
   STORAGE_CONCAT is a set of other storage types concatenated end-to-end
   STORAGE_LAYER is a set of storage types, representing the same address
     range; all will be written too, and will be read in turn until one
     succeeds
   STORAGE_REMAP is a layer on top of another store that remaps its blocks
   STORAGE_COPY is a memory snapshot of another store
   STORAGE_NETWORK means that the file is stored elsewhere on the
     network; all the remaining fields contan type-specific information.
   STORAGE_OTHER means none of these apply; and should be used when no
     meaningful answer can be given

   The vectors returned by file_get_storage_info encode each of the above
   (note that the first int is always the storage type).  There are four:
   ports, ints, offsets (off_t), and data (char); each type of store uses the
   following entries in each vector:

    -type-  -ports-  -ints-	     	     -offsets-	       -data-    -kids-
    device  DEVICE   TY, FL, BS, NR, NL, ML  NR * (OFFS, LEN)  NL + ML   -
    file    FILE     TY, FL, BS, NR, NL, ML  NR * (OFFS, LEN)  NL + ML   -
    memory  MEMOBJ   TY, FL, BS, NR, NL, ML  NR * (OFFS, LEN)  NL + ML   -
    task    TASK     TY, FL, BS, NR, NL, ML  NR * (OFFS, LEN)  NL + ML   -
      (the data for the above is a name (incl '\0') and a misc data block)
    null    -	     TY, FL		     SIZE	       -         -
      (BS is 1)
    ileave  -	     TY, FL, IL, NC	     -		       -         NC
      (BS is the LCM of its children; SIZE is the minimum of theirs * IL)
    concat  - 	     TY, FL, NC      	     -		       -	 NC
      (BS is the LCM of its children; SIZE is the sum of theirs)
    layer   - 	     TY, FL, NC      	     -		       -	 NC
      (BS is the LCM of its children; SIZE is the minimum of theirs)
    remap  - 	     TY, FL, NR      	     NR * (OFFS, LEN)  -	 1
      (BS and SIZE are that of the child)
    copy   -         TY, FL, SIZE	     -		       DATA	 -
      (DATA is preceded by padding to the next page boundary, and is
       SIZE bytes long itself)

  For ileave, concat, and layer, the children are encoded following the parent.
  The first int must always be TY.

  key: TY = type code, FL = flags, BS = block size, NR = num runs,
       NL = name len, ML = misc len, NC = num children,
       IL = interleave (bytes), SIZE = Size of storage (blocks),
       LEN = run length (blocks), OFFS = run offset (blocks),

  The NR * (OFFS, LEN) offsets for some of the types is the set of block
  ranges in the underlying address space that, concatenated, make up the
  contents of the storage -- for instance, doing file_get_storage_info on a
  file may return storage of type STORAGE_DEVICE, and the accompanying block
  ranges are the set of blocks on the given device that correspond to that
  file.  Any OFFS == -1 designates a hole in the address range.  Note that
  the total size (SIZE) for these types is the sum of their LEN's.

  The optional NAME returned by some types (if NL != 0) is a type specific
  name for the same object referenced by the port also returned.  E.g.:
    device -- The mach device name
    file   -- The file name (unreliable, as the root may not be the same)
    task   -- The pid
  Unless it is MACH_PORT_NULL, the port should generally be used instead of
  trying to regenerate it from the associated name, which is intended more for
  printing messages, etc.  */
enum file_storage_class
{
  STORAGE_OTHER,
  STORAGE_DEVICE,
  STORAGE_HURD_FILE,
  STORAGE_NETWORK,
  STORAGE_MEMORY,
  STORAGE_TASK,
  STORAGE_ZERO,
  STORAGE_CONCAT,
  STORAGE_INTERLEAVE,
  STORAGE_LAYER,
  STORAGE_REMAP,
  STORAGE_COPY,
};

/* Flags for the flags word returned by some types . */
#define STORAGE_MUTATED   0x00000001 /* data as stored is munged from file */

/*   Data types   */

#include <mach/task_info.h>
#include <mach/thread_info.h>
#ifndef THREAD_SCHED_INFO
#include <mach/policy.h>
#endif

/* Flags sent in proc_getprocinfo request. */
#define PI_FETCH_TASKINFO	0x0001
#define PI_FETCH_TASKEVENTS	0x0020
#define PI_FETCH_THREADS	0x0002
#define PI_FETCH_THREAD_BASIC	0x0004
#define PI_FETCH_THREAD_SCHED	0x0008
#define PI_FETCH_THREAD_WAITS	0x0010

struct procinfo
{
  int state;
  uid_t owner;
  pid_t ppid;
  pid_t pgrp;
  pid_t session;
  pid_t logincollection;
  int exitstatus;
  int sigcode;

  int nthreads;			/* size of pi_threadinfos */

  struct task_basic_info taskinfo;
  struct task_events_info taskevents;
#ifdef TASK_SCHED_TIMESHARE_INFO
  struct policy_timeshare_base timeshare_base_info;
#endif
  struct
    {
      int died;			/* this thread died in the middle of call */
      mach_msg_id_t rpc_block;	/* thread is blocked on this RPC */
      struct thread_basic_info pis_bi;
#ifdef THREAD_SCHED_INFO
      struct thread_sched_info pis_si;
#else
      struct policy_infos pis_pi;
#endif
    } threadinfos[0];
};
typedef int *procinfo_t;
typedef const int *const_procinfo_t;

/* Bits in struct procinfo  state: */
#define PI_STOPPED 0x00000001	/* Proc server thinks is stopped.  */
#define PI_EXECED  0x00000002	/* Has called proc_exec.  */
#define PI_WAITING 0x00000004	/* Process is waiting for child to exit */
#define PI_ORPHAN  0x00000008	/* Process group is orphaned.  */
#define PI_NOMSG   0x00000010	/* Process has no message port.  */
#define PI_SESSLD  0x00000020	/* Session leader.  */
#define PI_NOTOWNED 0x0000040	/* Process has no owner.  */
#define PI_NOPARENT 0x0000080	/* Hasn't identified a parent.  */
#define PI_ZOMBIE  0x00000100	/* Has no associated task.  */
#define PI_TRACED  0x00000200	/* Process is being traced */
#define PI_GETMSG  0x00000400	/* Process is blocked in proc_getmsgport. */
#define PI_LOGINLD 0x00000800	/* Process is leader of login collection */


/*   Conventions   */


/* st_fstype in struct stat and fsys_stb_type in fsys_statfsbuf is one of: */
#define FSTYPE_UFS     0x00000000 /* 4.x BSD Fast File System */
#define FSTYPE_NFS     0x00000001 /* Network File System ala Sun */
#define FSTYPE_GFS     0x00000002 /* GNU file system */
#define FSTYPE_LFS     0x00000003 /* Logging File System ala Sprite */
#define FSTYPE_SYSV    0x00000004 /* Old U*x filesystem ala System V */
#define FSTYPE_FTP     0x00000005 /* Transparent FTP */
#define FSTYPE_TAR     0x00000006 /* Transparent TAR */
#define FSTYPE_AR      0x00000007 /* Transparent AR */
#define FSTYPE_CPIO    0x00000008 /* Transparent CPIO */
#define FSTYPE_MSLOSS  0x00000009 /* MS-DOS */
#define FSTYPE_CPM     0x0000000a /* CP/M */
#define FSTYPE_HFS     0x0000000b /* Don't ask */
#define FSTYPE_DTFS    0x0000000c /* used by desktop to provide more info */
#define FSTYPE_GRFS    0x0000000d /* GNU Remote File System */
#define FSTYPE_TERM    0x0000000e /* GNU Terminal driver */
#define FSTYPE_DEV     0x0000000f /* GNU Special file server */
#define FSTYPE_PROC    0x00000010 /* /proc filesystem ala Version 9 */
#define FSTYPE_IFSOCK  0x00000011 /* PF_LOCAL socket naming point */
#define FSTYPE_AFS     0x00000012 /* Andrew File System 3.xx */
#define FSTYPE_DFS     0x00000013 /* Distributed File Sys (OSF) == AFS 4.xx */
#define FSTYPE_PROC9   0x00000014 /* /proc filesystem ala Plan 9 */
#define FSTYPE_SOCKET  0x00000015 /* io_t that isn't a file but a socket */
#define FSTYPE_MISC    0x00000016 /* generic trivfs server */
#define FSTYPE_EXT2FS  0x00000017 /* Linux filesystem by Remy Card */
#define FSTYPE_HTTP    0x00000018 /* Transparent HTTP */
#define FSTYPE_MEMFS   0x00000019 /* In-core filesystem */
#define FSTYPE_ISO9660 0x0000001a /* ISO9660 */

/* Standard port assignments for file_exec_paths and exec_* */
enum
  {
    INIT_PORT_CWDIR,
    INIT_PORT_CRDIR,
    INIT_PORT_AUTH,
    INIT_PORT_PROC,
    INIT_PORT_CTTYID,
    /* If MACH_PORT_NULL is given for the bootstrap port,
       the bootstrap port of the old task is used.  */
    INIT_PORT_BOOTSTRAP,
    INIT_PORT_MAX
  };

/* Standard ints for file_exec_paths and exec_* */
enum
  {
    INIT_UMASK,
    INIT_SIGMASK,
    INIT_SIGIGN,
    INIT_SIGPENDING,
    INIT_TRACEMASK,
    INIT_INT_MAX,
  };

#endif // _HURD_TYPES_H
