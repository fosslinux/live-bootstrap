/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __MES_GNU_SYSCALL_H
#define __MES_GNU_SYSCALL_H

#define _GNU_SOURCE 1
#define __USE_GNU 1

#include <mach/mach_types.h>
#include <mach/port.h>
#include <mach/message.h>
#include <gnu/hurd.h>
#include <gnu/hurd-types.h>

// mach/mach.defs
enum
  {
   SYS__task_terminate = 2008,
   SYS__vm_allocate = 2021,
   SYS__vm_statistics = 2030,
   SYS__task_get_special_port = 2058,
  };

// hurd/fsys.defs
enum
  {
   SYS__dir_lookup = 20018,
  };

// hurd/io.defs
enum
  {
   SYS__io_write = 21000,
   SYS__io_read,
  };

// hurd/process.defs
enum
  {
   SYS__proc_mark_exit = 24025,
  };

// hurd/startup.defs
enum
  {
   SYS__exec_startup_get_info = 30500,
  };

extern mach_msg_type_t mach_msg_type_int32;
extern mach_msg_type_t mach_msg_type_int64;
extern mach_msg_type_long_t mach_msg_type_pointer;

struct mach_msg
{
  mach_msg_header_t header;
};

struct mach_msg_1
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; int one;
};

struct mach_msg_2
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; int one;
  mach_msg_type_t type_two; int two;
};

struct mach_msg_loff_int
{
  mach_msg_header_t header;
  mach_msg_type_t type_one; loff_t one;
  mach_msg_type_t type_two; int two;
};

struct mach_msg_startup_info
{
  mach_msg_header_t header;
  mach_msg_type_t RetCodeType;
  kern_return_t RetCode;
  mach_msg_type_t user_entryType;
  vm_address_t user_entry;
  mach_msg_type_t phdrType;
  vm_address_t phdr;
  mach_msg_type_t phdr_sizeType;
  vm_size_t phdr_size;
  mach_msg_type_t stack_baseType;
  vm_address_t stack_base;
  mach_msg_type_t stack_sizeType;
  vm_size_t stack_size;
  mach_msg_type_t flagsType;
  int flags;
  mach_msg_type_long_t argvType;
  char *argv;
  mach_msg_type_long_t envpType;
  char *envp;
  mach_msg_type_long_t dtableType;
  mach_port_t *dtable;
  mach_msg_type_long_t portarrayType;
  mach_port_t *portarray;
  mach_msg_type_long_t intarrayType;
  int *intarray;
};

kern_return_t __syscall (mach_port_t port, int sys_call);
kern_return_t __syscall2 (mach_port_t port, int sys_call, int one, int two);
kern_return_t __syscall_get (mach_port_t port, int sys_call, mach_msg_header_t *message, size_t size);
kern_return_t __syscall_put (mach_port_t port, int sys_call, mach_msg_header_t *message, size_t size);

// mach.defs
kern_return_t __task_terminate (mach_port_t task);
kern_return_t __task_get_special_port (mach_port_t task, int which, mach_port_t *port);
kern_return_t __vm_allocate (mach_port_t task, vm_address_t *address, vm_size_t size, boolean_t anywhere);
kern_return_t __vm_statistics (mach_port_t task, vm_statistics_data_t *vm_stats);

// process.defs
kern_return_t __proc_mark_exit (mach_port_t process, int one, int two);
kern_return_t __exec_startup_get_data (mach_port_t bootstrap, struct hurd_startup_data *data);

// io.c
kern_return_t __io_write (io_t io_object, data_t data, mach_msg_type_number_t size, loff_t offset, vm_size_t *wrote);
kern_return_t __io_read (io_t io, data_t *data, mach_msg_type_number_t *read, loff_t offset, vm_size_t size);

#endif // __MES_GNU_SYSCALL_H
