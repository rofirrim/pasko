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

void PASKO_RUNTIME_PUBLIC __pasko_write_i64(int64_t num, int total_width);
void PASKO_RUNTIME_PUBLIC __pasko_write_f64(double num, int total_width,
                                            int frac_digits);
void PASKO_RUNTIME_PUBLIC __pasko_write_str(const char *);
void PASKO_RUNTIME_PUBLIC __pasko_write_bool(uint8_t b);

void PASKO_RUNTIME_PUBLIC __pasko_write_newline(void);

int64_t PASKO_RUNTIME_PUBLIC __pasko_read_i64(void);
double PASKO_RUNTIME_PUBLIC __pasko_read_f64(void);
void PASKO_RUNTIME_PUBLIC __pasko_read_newline(void);

#endif // PASK_RUNTIME_H
