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

PASKO_RUNTIME_PUBLIC void __pasko_write_i64(int64_t num, int total_width);
PASKO_RUNTIME_PUBLIC void __pasko_write_f64(double num, int total_width,
                                            int frac_digits);
PASKO_RUNTIME_PUBLIC void __pasko_write_str(const uint32_t *);
PASKO_RUNTIME_PUBLIC void __pasko_write_char(uint32_t);
PASKO_RUNTIME_PUBLIC void __pasko_write_bool(uint8_t b);

PASKO_RUNTIME_PUBLIC void __pasko_write_newline(void);

PASKO_RUNTIME_PUBLIC int64_t __pasko_read_i64(void);
PASKO_RUNTIME_PUBLIC double __pasko_read_f64(void);
PASKO_RUNTIME_PUBLIC void __pasko_read_newline(void);

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

PASKO_RUNTIME_PUBLIC void __pasko_pointer_new(void **ptr, uint64_t bytes);
PASKO_RUNTIME_PUBLIC void __pasko_pointer_dispose(void **ptr);

#endif // PASK_RUNTIME_H
