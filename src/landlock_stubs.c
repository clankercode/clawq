#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifdef __linux__

#include <linux/types.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <sys/prctl.h>

#ifndef __NR_landlock_create_ruleset
#define __NR_landlock_create_ruleset 444
#define __NR_landlock_add_rule 445
#define __NR_landlock_restrict_self 446
#endif

#ifndef LANDLOCK_CREATE_RULESET_VERSION
#define LANDLOCK_CREATE_RULESET_VERSION (1U << 0)
#endif

#ifndef LANDLOCK_ACCESS_FS_EXECUTE
#define LANDLOCK_ACCESS_FS_EXECUTE       (1ULL << 0)
#define LANDLOCK_ACCESS_FS_WRITE_FILE    (1ULL << 1)
#define LANDLOCK_ACCESS_FS_READ_FILE     (1ULL << 2)
#define LANDLOCK_ACCESS_FS_READ_DIR      (1ULL << 3)
#define LANDLOCK_ACCESS_FS_REMOVE_DIR    (1ULL << 4)
#define LANDLOCK_ACCESS_FS_REMOVE_FILE   (1ULL << 5)
#define LANDLOCK_ACCESS_FS_MAKE_CHAR     (1ULL << 6)
#define LANDLOCK_ACCESS_FS_MAKE_DIR      (1ULL << 7)
#define LANDLOCK_ACCESS_FS_MAKE_REG      (1ULL << 8)
#define LANDLOCK_ACCESS_FS_MAKE_SOCK     (1ULL << 9)
#define LANDLOCK_ACCESS_FS_MAKE_FIFO     (1ULL << 10)
#define LANDLOCK_ACCESS_FS_MAKE_BLOCK    (1ULL << 11)
#define LANDLOCK_ACCESS_FS_MAKE_SYM      (1ULL << 12)
#endif

#ifndef LANDLOCK_RULE_PATH_BENEATH
#define LANDLOCK_RULE_PATH_BENEATH 1
#endif

struct landlock_ruleset_attr {
  __u64 handled_access_fs;
} __attribute__((packed));

struct landlock_path_beneath_attr {
  __u64 allowed_access;
  __s32 parent_fd;
} __attribute__((packed));

CAMLprim value caml_landlock_available(value unit) {
  CAMLparam1(unit);
  long rc = syscall(__NR_landlock_create_ruleset, NULL, 0,
                    LANDLOCK_CREATE_RULESET_VERSION);
  CAMLreturn(Val_bool(rc >= 0));
}

CAMLprim value caml_landlock_create_ruleset(value handled_access_fs) {
  CAMLparam1(handled_access_fs);
  struct landlock_ruleset_attr attr;
  memset(&attr, 0, sizeof(attr));
  attr.handled_access_fs = (unsigned long long)Long_val(handled_access_fs);
  long fd = syscall(__NR_landlock_create_ruleset, &attr, sizeof(attr), 0);
  if (fd < 0)
    caml_failwith("landlock_create_ruleset failed");
  CAMLreturn(Val_int(fd));
}

CAMLprim value caml_landlock_add_rule_path(value ruleset_fd, value path,
                                           value access) {
  CAMLparam3(ruleset_fd, path, access);
  int parent_fd = open(String_val(path), O_PATH | O_CLOEXEC);
  if (parent_fd < 0)
    caml_failwith("landlock: cannot open path");
  struct landlock_path_beneath_attr attr;
  memset(&attr, 0, sizeof(attr));
  attr.allowed_access = (unsigned long long)Long_val(access);
  attr.parent_fd = parent_fd;
  long rc = syscall(__NR_landlock_add_rule, Int_val(ruleset_fd),
                    LANDLOCK_RULE_PATH_BENEATH, &attr, 0);
  close(parent_fd);
  if (rc < 0)
    caml_failwith("landlock_add_rule failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_landlock_restrict_self(value ruleset_fd) {
  CAMLparam1(ruleset_fd);
  if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) < 0)
    caml_failwith("prctl(NO_NEW_PRIVS) failed");
  long rc = syscall(__NR_landlock_restrict_self, Int_val(ruleset_fd), 0);
  close(Int_val(ruleset_fd));
  if (rc < 0)
    caml_failwith("landlock_restrict_self failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_landlock_close_fd(value fd) {
  CAMLparam1(fd);
  close(Int_val(fd));
  CAMLreturn(Val_unit);
}

#else /* non-Linux */

CAMLprim value caml_landlock_available(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_bool(0));
}

CAMLprim value caml_landlock_create_ruleset(value handled_access_fs) {
  CAMLparam1(handled_access_fs);
  caml_failwith("landlock not supported on this platform");
  CAMLreturn(Val_int(-1));
}

CAMLprim value caml_landlock_add_rule_path(value ruleset_fd, value path,
                                           value access) {
  CAMLparam3(ruleset_fd, path, access);
  caml_failwith("landlock not supported on this platform");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_landlock_restrict_self(value ruleset_fd) {
  CAMLparam1(ruleset_fd);
  caml_failwith("landlock not supported on this platform");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_landlock_close_fd(value fd) {
  CAMLparam1(fd);
  (void)fd;
  CAMLreturn(Val_unit);
}

#endif
