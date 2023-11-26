#define CLONE_NEWUSER 0x10000000
#define CLONE_NEWNS 0x00020000
#define MS_BIND 4096

#include <stdio.h>
#include <string.h>
#include <bootstrappable.h>

#include <sys/stat.h>

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

int set_map(int parent_id, char *path) {
  char *map_contents;
  char *parent_id_str;
  int fd = open(path, O_WRONLY, 0);
  require(fd != -1, "Cannot open map file");

  map_contents = calloc(37, sizeof(char));

  strcpy(map_contents, "0 ");
  parent_id_str = int2str(parent_id, 10, 0);
  strcat(map_contents, parent_id_str);
  strcat(map_contents, " 1");
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
  int uid = geteuid();
  int gid = getegid();
  unshare(CLONE_NEWUSER | CLONE_NEWNS);
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
  deny_setgroups();
  set_map(uid, "/proc/self/uid_map");
  set_map(gid, "/proc/self/gid_map");
  chroot (".");
  execve (argv[1], argv + 4 , envp);
}
