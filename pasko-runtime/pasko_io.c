#include <pasko_internal.h>
#include <pasko_runtime.h>

#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// File
typedef enum pasko_file_mode_t {
  PASKO_FILE_MODE_INSPECT,
  PASKO_FILE_MODE_GENERATE,
} pasko_file_mode_t;

struct pasko_file_t {
  FILE *file;
  void *buffer;
  pasko_file_mode_t mode : 1;
};

static pasko_file_t __pasko_file_output;
static pasko_file_t __pasko_file_input;

void __pasko_write_textfile_i64(pasko_file_t *f, int64_t num, int total_width) {
  if (total_width == 0) {
    fprintf(f->file, "%" PRId64, num);
  } else {
    fprintf(f->file, "%*" PRId64, total_width, num);
  }
}
void __pasko_write_i64(int64_t num, int total_width) {
  __pasko_write_textfile_i64(&__pasko_file_output, num, total_width);
}

void __pasko_write_textfile_f64(pasko_file_t *f, double num, int total_width,
                                int frac_digits) {
  if (total_width == 0) {
    fprintf(f->file, "%f", num);
  } else if (frac_digits == 0) {
    fprintf(f->file, "%*f", total_width, num);
  } else {
    fprintf(f->file, "%*.*f", total_width, frac_digits, num);
  }
}
void __pasko_write_f64(double num, int total_width, int frac_digits) {
  __pasko_write_textfile_f64(&__pasko_file_output, num, total_width,
                             frac_digits);
}

void __pasko_write_textfile_str(pasko_file_t *f, const uint32_t *str) {
  // FIXME: Assuming the environment is UTF-8.
  uint8_t *c = NULL;
  __pasko_utf32_to_utf8(str, &c);
  fprintf(f->file, "%s", (char *)c);
  __pasko_deallocate(c);
}
void __pasko_write_str(const uint32_t *str) {
  __pasko_write_textfile_str(&__pasko_file_output, str);
}

void __pasko_write_textfile_char(pasko_file_t *f, uint32_t c) {
  uint32_t str[2] = {c, 0};
  __pasko_write_textfile_str(f, str);
}
void __pasko_write_char(uint32_t c) {
  __pasko_write_textfile_char(&__pasko_file_output, c);
}

void __pasko_write_textfile_bool(pasko_file_t *f, uint8_t b) {
  fprintf(f->file, "%s", b ? "True" : "False");
}
void __pasko_write_bool(uint8_t b) {
  __pasko_write_textfile_bool(&__pasko_file_output, b);
}

void __pasko_write_textfile_newline(pasko_file_t *f) {
  fprintf(f->file, "%s", "\n");
}
void __pasko_write_newline(void) {
  __pasko_write_textfile_newline(&__pasko_file_output);
}

int64_t __pasko_read_textfile_i64(pasko_file_t *f) {
  int64_t result;
  int items = fscanf(f->file, "%" PRId64, &result);
  if (items != 1 || items == EOF) {
    __pasko_runtime_error("while reading integer");
  }
  return result;
}
int64_t __pasko_read_i64(void) {
  return __pasko_read_textfile_i64(&__pasko_file_input);
}

double __pasko_read_textfile_f64(pasko_file_t *f) {
  double result;
  int items = fscanf(f->file, "%lf", &result);
  if (items != 1 || items == EOF) {
    __pasko_runtime_error("while reading real");
  }
  return result;
}
double __pasko_read_f64(void) {
  return __pasko_read_textfile_f64(&__pasko_file_input);
}

void __pasko_read_textfile_newline(pasko_file_t *f) {
  int ch = fgetc(f->file);
  if ((unsigned char)ch != '\n' || ch == EOF) {
    __pasko_runtime_error("while reading newline");
  }
}
void __pasko_read_newline(void) {
  __pasko_read_textfile_newline(&__pasko_file_input);
}

static char __pasko_file_output_buffer[4];
static char __pasko_file_input_buffer[4];

void __pasko_init_io(int argc, char *argv[]) {
  (void)argc;
  (void)argv;

  __pasko_file_output.file = stdout;
  __pasko_file_output.buffer = __pasko_file_output_buffer;
  __pasko_file_output.mode = PASKO_FILE_MODE_GENERATE;

  __pasko_file_input.file = stdin;
  __pasko_file_input.buffer = __pasko_file_input_buffer;
  __pasko_file_input.mode = PASKO_FILE_MODE_INSPECT;
}
