#include <pasko_runtime.h>
#include <pasko_internal.h>

#include <stdio.h>
#include <stdlib.h>

void __pasko_runtime_error(const char *err) {
  fprintf(stderr, "Pasko Runtime Error: %s\n", err);
  abort();
}
