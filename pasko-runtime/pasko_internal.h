#ifndef PASKO_INTERNAL_H
#define PASKO_INTERNAL_H

#include <stddef.h>
#include <stdint.h>

void __pasko_runtime_error(const char *err);
void *__pasko_allocate(size_t bytes);
void __pasko_deallocate(void *p);
void __pasko_utf32_to_utf8(const uint32_t *input_buffer,
                           uint8_t **ref_out_buffer);

#endif // PASKO_INTERNAL_H
