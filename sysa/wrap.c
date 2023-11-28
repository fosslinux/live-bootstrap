#define CLONE_NEWUSER 0x10000000
#define CLONE_NEWNS 0x00020000
#define MS_BIND 4096
#define MNT_DETACH 0x00000002
#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <sys/stat.h>

#include <fcntl.h>
#include <unistd.h>

#ifdef __M2__

#include <bootstrappable.h>

#if __i386__

int unshare(int flags) {
  asm (
    "lea_ebx,[esp+DWORD] %4"
    "mov_ebx,[ebx]"
    "mov_eax, %310"
    "int !0x80"
  );
}

int geteuid() {
  asm (
    "mov_eax, %201"
    "int !0x80"
  );
}

int getegid() {
  asm (
    "mov_eax, %202"
    "int !0x80"
  );
}

int mount(
  char *source, char *target, char *filesystemtype,
  unsigned mountflags, void *data
) {
  asm (
    "DEFINE mov_esi,[esp+DWORD] 8BB424"
    "DEFINE mov_edi,[esp+DWORD] 8BBC24"
    "lea_ebx,[esp+DWORD] %20"
    "mov_ebx,[ebx]"
    "lea_ecx,[esp+DWORD] %16"
    "mov_ecx,[ecx]"
    "lea_edx,[esp+DWORD] %12"
    "mov_edx,[edx]"
    "mov_esi,[esp+DWORD] %8 ; mov esi, [esp+8]"
    "mov_edi,[esp+DWORD] %4 ; mov edi, [esp+4]"
    "mov_eax, %21"
    "int !0x80"
  );
}

int chroot(char *path) {
  asm (
    "lea_ebx,[esp+DWORD] %4"
    "mov_ebx,[ebx]"
    "mov_eax, %61"
    "int !0x80"
  );
}

#elif __x86_64__

int unshare(int flags) {
  asm (
    "lea_rdi,[rsp+DWORD] %8"
    "mov_rdi,[rdi]"
    "mov_rax, %272"
    "syscall"
  );
}

int geteuid() {
  asm (
    "mov_rax, %107"
    "syscall"
  );
}

int getegid() {
  asm (
    "mov_rax, %108"
    "syscall"
  );
}

int mount(
  char *source, char *target, char *filesystemtype,
  unsigned mountflags, void *data
) {
  asm (
    "DEFINE mov_r8,[rsp+DWORD] 4C8D8424"
    "DEFINE mov_r10,[rsp+DWORD] 4C8B9424"
    "lea_rdi,[rsp+DWORD] %40"
    "mov_rdi,[rdi]"
    "lea_rsi,[rsp+DWORD] %32"
    "mov_rsi,[rsi]"
    "lea_rdx,[rsp+DWORD] %24"
    "mov_rdx,[rdx]"
    "mov_r10,[rsp+DWORD] %16"
    "mov_r8,[rsp+DWORD] %8"
    "mov_rax, %165"
    "syscall"
  );
}

int chroot(char *path) {
  asm (
    "lea_rdi,[rsp+DWORD] %8"
    "mov_rdi,[rdi]"
    "mov_rax, %161"
    "syscall"
  );
}

#else

#error arch not supported

#endif

#else

// From bootstrappable.c in M2libc

void require(int bool, char* error)
{
  if(!bool)
  {
    fputs(error, stderr);
    exit(EXIT_FAILURE);
  }
}

extern int unshare(int flags);

extern int mount(const char *source, const char *target,
  const char *filesystemtype, unsigned long mountflags, const void *data);

#endif

void set_map(int parent_id, char *path) {
  int fd = open(path, O_WRONLY, 0);
  require(fd != -1, "Cannot open map file");

  char *map_contents = calloc(38, sizeof(char));

#ifdef __M2__
  strcpy(map_contents, "0 ");
  char *parent_id_str = int2str(parent_id, 10, 0);
  strcat(map_contents, parent_id_str);
  strcat(map_contents, " 1");
#else
  snprintf(map_contents, 38, "0 %i 1", parent_id);
#endif
  write(fd, map_contents, strlen(map_contents));
  write(STDOUT_FILENO, map_contents, strlen(map_contents));
  free(map_contents);
  close(fd);
}

void touch(char *path) {
  int fd = open(path, O_CREAT, 0777);
  require(fd != -1, "Cannot open file");
  close(fd);
}

void deny_setgroups() {
  int fd = open("/proc/self/setgroups", O_WRONLY, 0777);
  require(fd != -1, "Failed to open /proc/self/setgroups");
  write(fd, "deny", 4);
  close(fd);
}

int main(int argc, char **argv, char **envp) {
  require(argc > 1, "Expected at least one argument: command");
  char *cwd = get_current_dir_name();
  /* Do nothing if cwd is already root */
  if (strcmp(cwd, "/")) {
    int uid = geteuid();
    int gid = getegid();
    /* Don't create a user and mount namespace if we are already root */
    if (uid != 0) {
      require(unshare(CLONE_NEWUSER | CLONE_NEWNS) == 0, "Failed to create user and mount namespaces");
      /* Prevent the use of setgroups and make gid_map writeable */
      deny_setgroups();
      /* Map the root user in the user namespace to our user id */
      set_map(uid, "/proc/self/uid_map");
      /* Map the root group in the user namespace to our group id */
      set_map(gid, "/proc/self/gid_map");
    }
    mkdir ("dev", 0755);
    touch ("dev/null");
    mount ("/dev/null", "dev/null", "", MS_BIND, NULL);
    touch ("dev/zero");
    mount ("/dev/zero", "dev/zero", "", MS_BIND, NULL);
    touch ("dev/random");
    mount ("/dev/random", "dev/random", "", MS_BIND, NULL);
    touch ("dev/urandom");
    mount ("/dev/urandom", "dev/urandom", "", MS_BIND, NULL);
    touch ("dev/ptmx");
    mount ("/dev/ptmx", "dev/ptmx", "", MS_BIND, NULL);
    touch ("dev/tty");
    mount ("/dev/tty", "dev/tty", "", MS_BIND, NULL);
    mkdir ("dev/shm", 0755);
    mount ("tmpfs", "dev/shm", "tmpfs", 0, NULL);
    mkdir ("proc", 0755);
    mount ("proc", "proc", "proc", 0, NULL);
    mkdir ("sys", 0755);
    mount ("/sys", "sys", "", MS_BIND, NULL);
    mkdir ("tmp", 0755);
    mount ("tmpfs", "tmp", "tmpfs", 0, NULL);
    chroot (".");
  }
  free(cwd);
  return execve (argv[1], argv + sizeof(char *) , envp);
}
