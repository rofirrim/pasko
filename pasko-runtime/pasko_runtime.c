#include <inttypes.h>
#include <pasko_runtime.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void __pasko_runtime_error(const char *err) {
  fprintf(stderr, "Pasko Runtime Error: %s\n", err);
  exit(EXIT_FAILURE);
}

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

void __pasko_utf32_to_utf8(const uint32_t *input_buffer, uint8_t **ref_out_buffer) {
  if (!input_buffer) {
    __pasko_runtime_error("when converting UTF-32 to UTF-8: null input buffer");
  }
  if (!ref_out_buffer) {
    __pasko_runtime_error("when converting UTF-32 to UTF-8: null output buffer reference");
  }

  // Count bytes.
  size_t bytes_out = 1;
  {
    const uint32_t *p = input_buffer;
    while (*p != 0) {
      uint32_t cp = *p;
      if (cp < 0x80) {
        bytes_out += 1;
      } else if (cp < 0x800) {
        bytes_out += 2;
      } else if (cp < 0x10000) {
        bytes_out += 3;
      } else if (cp < 0x200000) {
        bytes_out += 4;
      } else {
        __pasko_runtime_error(
            "when converting UTF-32 to UTF-8: invalid codepoint");
      }
      p++;
    }
  }

  uint8_t *out_buffer = __pasko_allocate(bytes_out);

  const uint32_t *p = input_buffer;
  uint8_t *q = out_buffer;
  while (*p != 0) {
    uint32_t cp = *p;
    if (cp < 0x80) {
      *q = cp & 0xff;
    } else if (cp < 0x800) {
      *q = 0xc0 | (cp >> 6); q++;
      *q = 0x80 | (cp & 0x3f);
    } else if (cp < 0x10000) {
      *q = 0xe0 | (cp >> 12); q++;
      *q = 0x80 | ((cp >> 6) & 0x3f); q++;
      *q = 0x80 | (cp & 0x3f);
    } else if (cp < 0x200000) {
      *q = 0xf0 | (cp >> 18); q++;
      *q = 0x80 | ((cp >> 12) & 0x3f); q++;
      *q = 0x80 | ((cp >> 6) & 0x3f); q++;
      *q = 0x80 | (cp & 0x3f);
    } else {
      // Should not happen, but just in case.
      __pasko_runtime_error(
          "when converting UTF-32 to UTF-8: invalid codepoint during output");
    }
    p++;
    q++;
  }
  *q = 0;

  *ref_out_buffer = out_buffer;
}

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
  printf("%s", (char*)c);
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


