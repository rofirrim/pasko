#ifndef PASKO_RUNTIME_H
#define PASK_RUNTIME _H

#include <stdint.h>

#if defined _WIN32 || defined __CYGWIN__
#ifdef BUILDING_PASKO_RUNTIME
#define PASKO_RUNTIME_PUBLIC __declspec(dllexport)
#else
#define PASKO_RUNTIME_PUBLIC __declspec(dllimport)
#endif
#else
#ifdef BUILDING_PASKO_RUNTIME
#define PASKO_RUNTIME_PUBLIC __attribute__((visibility("default")))
#else
#define PASKO_RUNTIME_PUBLIC
#endif
#endif

typedef struct pasko_file_t pasko_file_t;

PASKO_RUNTIME_PUBLIC void __pasko_init(int argc, char *argv[],
                                       int num_program_params,
                                       char *program_params[],
                                       int num_global_files,
                                       pasko_file_t **global_files[]);
PASKO_RUNTIME_PUBLIC void __pasko_finish(int num_global_files,
                                         pasko_file_t **global_files[]);

// I/O
// textfile output
PASKO_RUNTIME_PUBLIC void
__pasko_write_textfile_i64(pasko_file_t *file, int64_t num, int64_t total_width);
PASKO_RUNTIME_PUBLIC void __pasko_write_textfile_f64(pasko_file_t *file,
                                                     double num,
                                                     int64_t total_width,
                                                     int64_t frac_digits);
PASKO_RUNTIME_PUBLIC void __pasko_write_textfile_str(pasko_file_t *file,
                                                     const uint32_t *);
PASKO_RUNTIME_PUBLIC void __pasko_write_textfile_char(pasko_file_t *file,
                                                      uint32_t);
PASKO_RUNTIME_PUBLIC void __pasko_write_textfile_bool(pasko_file_t *file,
                                                      uint8_t b);

PASKO_RUNTIME_PUBLIC void __pasko_write_textfile_newline(pasko_file_t *file);

// textfile input
PASKO_RUNTIME_PUBLIC int64_t __pasko_read_textfile_i64(pasko_file_t *);
PASKO_RUNTIME_PUBLIC double __pasko_read_textfile_f64(pasko_file_t *);
PASKO_RUNTIME_PUBLIC void __pasko_read_textfile_newline(pasko_file_t *);

// input
PASKO_RUNTIME_PUBLIC pasko_file_t *__pasko_get_input(void);

// output
PASKO_RUNTIME_PUBLIC pasko_file_t *__pasko_get_output(void);

// files i/o
PASKO_RUNTIME_PUBLIC void __pasko_reset_file(pasko_file_t *f, uint64_t bytes);
PASKO_RUNTIME_PUBLIC void __pasko_reset_textfile(pasko_file_t *f);
PASKO_RUNTIME_PUBLIC void __pasko_rewrite_file(pasko_file_t *f);
PASKO_RUNTIME_PUBLIC void __pasko_rewrite_textfile(pasko_file_t *f);
PASKO_RUNTIME_PUBLIC void __pasko_get_file(pasko_file_t *f, uint64_t bytes);
PASKO_RUNTIME_PUBLIC void __pasko_put_file(pasko_file_t *f, uint64_t bytes);
PASKO_RUNTIME_PUBLIC void __pasko_get_textfile(pasko_file_t *f);
PASKO_RUNTIME_PUBLIC void __pasko_put_textfile(pasko_file_t *f);

PASKO_RUNTIME_PUBLIC void *__pasko_buffer_var_file(pasko_file_t *f, uint64_t bytes);
PASKO_RUNTIME_PUBLIC uint32_t *__pasko_buffer_var_textfile(pasko_file_t *f);

// files inquiry
PASKO_RUNTIME_PUBLIC uint8_t __pasko_eof_file(pasko_file_t* f);
PASKO_RUNTIME_PUBLIC uint8_t __pasko_eof_textfile(pasko_file_t* f);

PASKO_RUNTIME_PUBLIC uint8_t __pasko_eoln_textfile(pasko_file_t* f);

// Set type
typedef struct pasko_set_t pasko_set_t;
PASKO_RUNTIME_PUBLIC pasko_set_t *__pasko_set_new(uint64_t N, int64_t *values);
PASKO_RUNTIME_PUBLIC void __pasko_set_dispose(pasko_set_t *s);
PASKO_RUNTIME_PUBLIC pasko_set_t *__pasko_set_union(pasko_set_t *a,
                                                    pasko_set_t *b);
PASKO_RUNTIME_PUBLIC pasko_set_t *__pasko_set_intersection(pasko_set_t *a,
                                                           pasko_set_t *b);
PASKO_RUNTIME_PUBLIC pasko_set_t *__pasko_set_difference(pasko_set_t *a,
                                                         pasko_set_t *b);
PASKO_RUNTIME_PUBLIC uint8_t __pasko_set_contains(pasko_set_t *a,
                                                  int64_t value);
PASKO_RUNTIME_PUBLIC pasko_set_t *__pasko_set_copy(pasko_set_t *src);
PASKO_RUNTIME_PUBLIC uint8_t __pasko_set_equal(pasko_set_t *a, pasko_set_t *b);
PASKO_RUNTIME_PUBLIC uint8_t __pasko_set_not_equal(pasko_set_t *a,
                                                   pasko_set_t *b);
PASKO_RUNTIME_PUBLIC uint8_t __pasko_set_is_subset(pasko_set_t *a,
                                                   pasko_set_t *b);

// Pointers
PASKO_RUNTIME_PUBLIC void __pasko_pointer_new(void **ptr, uint64_t bytes);
PASKO_RUNTIME_PUBLIC void __pasko_pointer_dispose(void **ptr);

// Math
PASKO_RUNTIME_PUBLIC double __pasko_arctan_f64(double);
PASKO_RUNTIME_PUBLIC double __pasko_cos_f64(double);
PASKO_RUNTIME_PUBLIC double __pasko_exp_f64(double);
PASKO_RUNTIME_PUBLIC double __pasko_ln_f64(double);
PASKO_RUNTIME_PUBLIC double __pasko_sin_f64(double);

#endif // PASK_RUNTIME_H
