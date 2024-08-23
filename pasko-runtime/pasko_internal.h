#ifndef PASKO_INTERNAL_H
#define PASKO_INTERNAL_H

#include "pasko_runtime.h"

#include <stddef.h>
#include <stdint.h>

__attribute__((noreturn)) void __pasko_runtime_error(const char *err);
void *__pasko_allocate(size_t bytes);
void __pasko_deallocate(void *p);
void* __pasko_reallocate(void *p, size_t new_size);
char* __pasko_strdup(const char *p);
void __pasko_utf32_to_utf8(const uint32_t *input_buffer,
                           uint8_t **ref_out_buffer);
void __pasko_utf32_to_utf8_n(const uint32_t *input_buffer,
                           uint8_t **ref_out_buffer,
                           uint64_t num_chars);

void __pasko_init_io(int argc, char *argv[], int num_program_params,
                     char *program_params[], int num_global_files,
                     pasko_file_t **global_files[]);
void __pasko_finish_io(int num_global_files, pasko_file_t **global_files[]);

void __pasko_ignoring_argument(const char *arg, const char *reason);

#ifndef PASKO_RUNTIME_CAN_USE_STDC_MEMORY
#pragma GCC poison malloc
#pragma GCC poison realloc
#pragma GCC poison free
#pragma GCC poison strdup
#endif

#endif // PASKO_INTERNAL_H
