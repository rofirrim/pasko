#include <pasko_internal.h>
#include <pasko_runtime.h>

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


void __pasko_write_i64(int64_t num, int total_width) {
  if (total_width == 0) {
    printf("%" PRId64, num);
  } else {
    printf("%*" PRId64, total_width, num);
  }
}
void __pasko_write_f64(double num, int total_width, int frac_digits) {
  if (total_width == 0) {
    printf("%f", num);
  } else if (frac_digits == 0) {
    printf("%*f", total_width, num);
  } else {
    printf("%*.*f", total_width, frac_digits, num);
  }
}
void __pasko_write_str(const uint32_t *str) {
  // FIXME: Assuming the environment is UTF-8.
  uint8_t *c = NULL;
  __pasko_utf32_to_utf8(str, &c);
  printf("%s", (char *)c);
  __pasko_deallocate(c);
}
void __pasko_write_char(uint32_t c) {
  uint32_t str[2] = {c, 0};
  __pasko_write_str(str);
}

void __pasko_write_bool(uint8_t b) { printf("%s", b ? "True" : "False"); }

void __pasko_write_newline(void) { printf("%s", "\n"); }

int64_t __pasko_read_i64(void) {
  int64_t result;
  int items = scanf("%" PRId64, &result);
  if (items != 1 || items == EOF) {
    __pasko_runtime_error("while reading integer");
  }
  return result;
}
double __pasko_read_f64(void) {
  double result;
  int items = scanf("%lf", &result);
  if (items != 1 || items == EOF) {
    __pasko_runtime_error("while reading real");
  }
  return result;
}
void __pasko_read_newline(void) {
  int ch = getchar();
  if ((unsigned char)ch != '\n' || ch == EOF) {
    __pasko_runtime_error("while reading newline");
  }
}
