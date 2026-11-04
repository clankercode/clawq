#define _POSIX_C_SOURCE 200809L
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>
#include <time.h>

CAMLprim value clawq_relay_local_format(value format, value seconds)
{
  CAMLparam2(format, seconds);
  time_t instant = (time_t)Long_val(seconds);
  struct tm local;
  char output[256];

  tzset();
  if (localtime_r(&instant, &local) == NULL ||
      strftime(output, sizeof(output), String_val(format), &local) == 0)
    caml_invalid_argument("relay local time cannot be formatted");
  CAMLreturn(caml_copy_string(output));
}

/* Test cleanup must restore an absent TZ as absent, and reset libc's cached
   timezone too. Unix.putenv cannot remove a variable on OCaml 5.1. */
CAMLprim value clawq_relay_restore_timezone(value previous)
{
  CAMLparam1(previous);
  int result = Is_block(previous)
    ? setenv("TZ", String_val(Field(previous, 0)), 1)
    : unsetenv("TZ");
  if (result != 0)
    caml_failwith("relay test timezone restoration failed");
  tzset();
  CAMLreturn(Val_unit);
}
