#define PASKO_RUNTIME_CAN_USE_STDC_MEMORY

#include <pasko_runtime.h>
#include <pasko_internal.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void* __pasko_allocate(size_t bytes) {
  void *p = malloc(bytes);
  if (!p) {
    __pasko_runtime_error("allocating memory");
  }
  return p;
}

void __pasko_deallocate(void *p) {
  free(p);
}

void *__pasko_reallocate(void *p, size_t new_size) {
  return realloc(p, new_size);
}

void __pasko_pointer_new(void **ptr, uint64_t bytes)
{
  *ptr = __pasko_allocate(bytes);
}

void __pasko_pointer_dispose(void **ptr) {
  __pasko_deallocate(*ptr);
}

char* __pasko_strdup(const char* c) {
  return strdup(c);
}
