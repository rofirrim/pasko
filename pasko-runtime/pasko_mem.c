#include <pasko_runtime.h>
#include <pasko_internal.h>

#include <stdint.h>
#include <stdlib.h>

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

void __pasko_pointer_new(void **ptr, uint64_t bytes)
{
  *ptr = __pasko_allocate(bytes);
}

void __pasko_pointer_dispose(void **ptr) {
  __pasko_deallocate(*ptr);
}
