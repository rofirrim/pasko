#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

#include <pasko_internal.h>
#include <pasko_runtime.h>

// Buffer of characters + EOF.
typedef struct pasko_buffer_textfile_t {
  FILE *src;
  int *buffer;
  size_t size;
  size_t start;
  size_t end;
  // assert(start < end)

} pasko_buffer_textfile_t;

static void __pasko_buffer_textfile_init(pasko_buffer_textfile_t *b, FILE *f) {
  assert(f != NULL);
  b->src = f;

  b->buffer = NULL;
  b->size = 0;
  b->start = 0;
  b->end = 0;
}

static pasko_buffer_textfile_t *__pasko_buffer_textfile_new(FILE *f) {
  pasko_buffer_textfile_t *b = __pasko_allocate(sizeof(*b));
  __pasko_buffer_textfile_init(b, f);
  return b;
}

static void __pasko_buffer_textfile_finish(pasko_buffer_textfile_t *b) {
  __pasko_deallocate(b->buffer);
  __pasko_deallocate(b);
}

enum { INITIAL_TEXTFILE_BUFFER_SIZE = 16, INITIAL_UTF8_BUFFER_SIZE = 16 };

static int __pasko_buffer_textfile_peek(pasko_buffer_textfile_t *b) {
  assert(b->end >= b->start);

  size_t num_queued_items = b->end - b->start;

  if (num_queued_items < 1) {
    assert(b->end != SIZE_MAX);
    // Check if there is enough room in the current buffer. If not reallocate.
    if (b->end + 1 > b->size) {
      size_t new_size = b->size ? b->size * 2 : INITIAL_TEXTFILE_BUFFER_SIZE;
      void *r = __pasko_reallocate(b->buffer, new_size * sizeof(int));
      if (!r)
        __pasko_runtime_error("failed allocation of textfile buffer");
      b->buffer = r;
      b->size = new_size;
    }
    b->buffer[b->end] = fgetc(b->src);
    b->end++;
  }

  return b->buffer[b->start];
}

static void __pasko_buffer_textfile_skip(pasko_buffer_textfile_t *b) {
  (void)__pasko_buffer_textfile_peek(b);

  b->start++;

  if (b->start == b->end) {
    b->start = 0;
    b->end = 0;
  }
}

// Buffer of (Pascal) chars.
typedef struct pasko_buffer_char_t {
  pasko_buffer_textfile_t *src;
  uint32_t *buffer;
  size_t size;
  size_t start;
  size_t end;
  // assert(start < end)

} pasko_buffer_char_t;

static void __pasko_buffer_char_init(pasko_buffer_char_t *b,
                                     pasko_buffer_textfile_t *src) {
  assert(src != NULL);
  b->src = src;

  b->buffer = NULL;
  b->size = 0;
  b->start = 0;
  b->end = 0;
}

static pasko_buffer_char_t *
__pasko_buffer_char_new(pasko_buffer_textfile_t *src) {
  pasko_buffer_char_t *b = __pasko_allocate(sizeof(*b));
  __pasko_buffer_char_init(b, src);
  return b;
}

#define UTF8_EOF UINT32_MAX
#define UNICODE_REPLACEMENT_CHAR 0xFFFD

static uint32_t __pasko_buffer_char_peek(pasko_buffer_char_t *b) {
  assert(b->end >= b->start);

  size_t num_queued_items = b->end - b->start;

  if (num_queued_items < 1) {
    assert(b->end != SIZE_MAX);
    // Check if there is enough room in the current buffer. If not reallocate.
    if (b->end + 1 > b->size) {
      size_t new_size = b->size ? b->size * 2 : INITIAL_UTF8_BUFFER_SIZE;
      void *r = __pasko_reallocate(b->buffer, new_size * sizeof(uint32_t));
      if (!r)
        __pasko_runtime_error("failed allocation of textfile buffer");
      b->buffer = r;
      b->size = new_size;
    }

    // FIXME: This is a mess.
    // We need to peek up to 4 bytes.
    int b0 = __pasko_buffer_textfile_peek(b->src);
    if (b0 == EOF) {
      // We're done.
      b->buffer[b->end] = UTF8_EOF;
      // Do not skip the input.
    } else if (b0 < 0x80) {
      // 1 byte.
      b->buffer[b->end] = (unsigned char)b0;
      // Done
      __pasko_buffer_textfile_skip(b->src);
    } else if (((unsigned char)b0 >> 5) == 0x6) {
      // 2 bytes.
      // Read another byte.
      __pasko_buffer_textfile_skip(b->src);
      int b1 = __pasko_buffer_textfile_peek(b->src);
      if (b1 == EOF || ((unsigned char)b1 >> 6) != 0x2) {
        b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
      } else {
        uint32_t value = (unsigned char)b1 & 0xf;
        value |= ((((unsigned char)b1 >> 4) & 0x3) |
                  (((unsigned char)b0 & 0x3) << 2))
                 << 4;
        value |= (((unsigned char)b0 >> 2) & 0x7) << 8;
        b->buffer[b->end] = value;
      }
      // Done
      __pasko_buffer_textfile_skip(b->src);
    } else if ((unsigned char)b0 >> 4 == 0xe) {
      // 3 bytes
      // Read another byte.
      __pasko_buffer_textfile_skip(b->src);
      int b1 = __pasko_buffer_textfile_peek(b->src);
      if (b1 == EOF || ((unsigned char)b1 >> 6) != 0x2) {
        b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
      } else {
        // Read another byte.
        __pasko_buffer_textfile_skip(b->src);
        int b2 = __pasko_buffer_textfile_peek(b->src);
        if (b2 == EOF || ((unsigned char)b2 >> 6) != 0x2) {
          b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
        } else {
          // We can now build the value.
          uint32_t value = (unsigned char)b2 & 0xf;
          value |= ((((unsigned char)b2 >> 4) & 0x3) |
                    (((unsigned char)b1 & 0x3) << 2))
                   << 4;
          value |= (((unsigned char)b1 >> 2) & 0xf) << 8;
          value |= ((unsigned char)b0 & 0xf) << 12;
          b->buffer[b->end] = value;
        }
      }
      // Done
      __pasko_buffer_textfile_skip(b->src);
    } else if ((unsigned char)b0 >> 3 == 0x1e) {
      // 4 bytes
      // Read another byte.
      __pasko_buffer_textfile_skip(b->src);
      int b1 = __pasko_buffer_textfile_peek(b->src);
      if (b1 == EOF || ((unsigned char)b1 >> 6) != 0x2) {
        b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
      } else {
        // Read another byte.
        __pasko_buffer_textfile_skip(b->src);
        int b2 = __pasko_buffer_textfile_peek(b->src);
        if (b2 == EOF || ((unsigned char)b2 >> 6) != 0x2) {
          b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
        } else {
          __pasko_buffer_textfile_skip(b->src);
          int b3 = __pasko_buffer_textfile_peek(b->src);
          if (b3 == EOF || ((unsigned char)b3 >> 6) != 0x2) {
            b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
          } else {
            uint32_t value = (unsigned char)b3 & 0xf;
            value |= ((((unsigned char)b3 >> 4) & 0x3) |
                      (((unsigned char)b2 & 0x3) << 2))
                     << 4;
            value |= (((unsigned char)b2 >> 2) & 0xf) << 8;
            value |= ((unsigned char)b1 & 0xf) << 12;
            value |= ((((unsigned char)b1 >> 4) & 0x3) |
                      ((unsigned char)b0 & 0x3) << 2)
                     << 16;
            value |= ((unsigned char)b0 & 0x4) << 20;
            b->buffer[b->end] = value;
          }
        }
      }
      // Done
      __pasko_buffer_textfile_skip(b->src);
    } else {
      b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
      __pasko_buffer_textfile_skip(b->src);
    }
    b->end++;
  }

  return b->buffer[b->start];
}

static uint32_t *__pasko_buffer_char_peek_addr(pasko_buffer_char_t *b) {
  (void)__pasko_buffer_char_peek(b);
  return &b->buffer[b->start];
}

static void __pasko_buffer_char_skip(pasko_buffer_char_t *b) {
  (void)__pasko_buffer_char_peek(b);

  b->start++;

  if (b->start == b->end) {
    b->start = 0;
    b->end = 0;
  }
}

static void __pasko_buffer_char_finish(pasko_buffer_char_t *b) {
  __pasko_buffer_textfile_finish(b->src);
  __pasko_deallocate(b->buffer);
  __pasko_deallocate(b);
}

// File
typedef enum pasko_file_mode_t {
  PASKO_FILE_MODE_NONE,
  PASKO_FILE_MODE_INSPECT,
  PASKO_FILE_MODE_GENERATE,
} pasko_file_mode_t;

struct pasko_file_t {
  FILE *file;
  union {
    pasko_buffer_char_t *read_buffer;
    void *buffer_var;
  };
  pasko_file_mode_t mode : 2;
  bool is_textfile : 1;
};

static pasko_file_t __pasko_file_output;
static pasko_file_t __pasko_file_input;

static void __pasko_check_read(pasko_file_t *f) {
  if (f->mode != PASKO_FILE_MODE_INSPECT)
    __pasko_runtime_error("invalid file mode for operation");
}
static void __pasko_check_write(pasko_file_t *f) {
  if (f->mode != PASKO_FILE_MODE_GENERATE)
    __pasko_runtime_error("invalid file mode for operation");
}

static const uint64_t log10_table[] = {UINT64_C(10),
                                       UINT64_C(100),
                                       UINT64_C(1000),
                                       UINT64_C(10000),
                                       UINT64_C(100000),
                                       UINT64_C(1000000),
                                       UINT64_C(10000000),
                                       UINT64_C(100000000),
                                       UINT64_C(1000000000),
                                       UINT64_C(10000000000),
                                       UINT64_C(100000000000),
                                       UINT64_C(1000000000000),
                                       UINT64_C(10000000000000),
                                       UINT64_C(100000000000000),
                                       UINT64_C(1000000000000000),
                                       UINT64_C(10000000000000000),
                                       UINT64_C(100000000000000000),
                                       UINT64_C(1000000000000000000),
                                       UINT64_C(10000000000000000000)};

static unsigned __pasko_i64_size(int64_t x) {
  if (x == INT64_MIN)
    __pasko_runtime_error("integer out of range");

  uint64_t a = x < 0 ? -x : x;

  // [lower, upper)
  int lower = 0;
  int upper = 19;
  do {
    int mid_point = (upper + lower) / 2;
    if (a < log10_table[mid_point]) {
      upper = mid_point;
    } else if (log10_table[mid_point] <= a) {
      lower = mid_point + 1;
    }
  } while (lower < upper);

  int size = lower + 1;
  return size;
}

void __pasko_write_textfile_i64(pasko_file_t *f, int64_t num,
                                int64_t total_width) {
  __pasko_check_write(f);
  if (total_width < 0)
    total_width = 0;
  unsigned int_digits = __pasko_i64_size(num);

  if (num == INT64_MIN)
    __pasko_runtime_error("integer out of range");

  int64_t a = num < 0 ? -num : num;

  if ((uint64_t)total_width >= int_digits + 1) {
    int num_spaces = total_width - int_digits - 1;
    for (int i = 0; i < num_spaces; i++) {
      fprintf(f->file, " ");
    }
    if (num < 0)
      fprintf(f->file, "-");
    else
      fprintf(f->file, " ");
  } else {
    //
    if (num < 0)
      fprintf(f->file, "-");
  }
  fprintf(f->file, "%ld", a);
}

void __pasko_write_textfile_f64(pasko_file_t *f, double num,
                                int64_t total_width, int64_t frac_digits) {
  __pasko_check_write(f);
  if (total_width < 0)
    total_width = 0;
  if (frac_digits < 0)
    frac_digits = 0;
  // FIXME: We should implement the ISO Pascal algorithm.
  if (total_width == 0) {
    fprintf(f->file, "%f", num);
  } else if (frac_digits == 0) {
    fprintf(f->file, "%*f", (int)total_width, num);
  } else {
    fprintf(f->file, "%*.*f", (int)total_width, (int)frac_digits, num);
  }
}

void __pasko_write_textfile_str(pasko_file_t *f, const uint32_t *str, uint64_t num_chars) {
  __pasko_check_write(f);
  // FIXME: Assuming the environment is UTF-8.
  uint8_t *c = NULL;
  __pasko_utf32_to_utf8_n(str, &c, num_chars);
  fprintf(f->file, "%s", (char *)c);
  __pasko_deallocate(c);
}

void __pasko_write_textfile_char(pasko_file_t *f, uint32_t c) {
  __pasko_write_textfile_str(f, &c, 1);
}

void __pasko_write_textfile_bool(pasko_file_t *f, uint8_t b) {
  __pasko_check_write(f);
  fprintf(f->file, "%s", b ? "True" : "False");
}

void __pasko_write_textfile_newline(pasko_file_t *f) {
  __pasko_check_write(f);
  fprintf(f->file, "%s", "\n");
}

static bool is_whitespace(uint32_t c) { return c == ' '; }

static void __pasko_skip_whitespace(pasko_file_t *f) {
  uint32_t v = __pasko_buffer_char_peek(f->read_buffer);
  while (is_whitespace(v)) {
    __pasko_buffer_char_skip(f->read_buffer);
    v = __pasko_buffer_char_peek(f->read_buffer);
  }
}

int64_t __pasko_read_textfile_i64(pasko_file_t *f) {
  __pasko_check_read(f);
  uint64_t tmp_result = 0;

  __pasko_skip_whitespace(f);
  uint32_t v = __pasko_buffer_char_peek(f->read_buffer);
  bool neg = false;
  if (v == '+' || v == '-') {
    if (v == '-')
      neg = true;
    __pasko_buffer_char_skip(f->read_buffer);
    v = __pasko_buffer_char_peek(f->read_buffer);
  }
  if ('0' <= v && v <= '9') {
    tmp_result = 0;
    do {
      tmp_result = (v - '0') + tmp_result * 10;
      if (tmp_result >= INT64_MAX) {
        __pasko_runtime_error("integer is too large");
      }
      __pasko_buffer_char_skip(f->read_buffer);
      v = __pasko_buffer_char_peek(f->read_buffer);
    } while ('0' <= v && v <= '9');
    int64_t result = tmp_result;
    if (neg)
      result = -result;
    return result;
  }
  __pasko_runtime_error("incorrectly formatted integer");
  return 0;
}

double __pasko_read_textfile_f64(pasko_file_t *f) {
  __pasko_check_read(f);
  enum { MAX_LITERAL_LENGTH = 128 };
  char c[MAX_LITERAL_LENGTH];
  int position = 0;

  __pasko_skip_whitespace(f);
  uint32_t v = __pasko_buffer_char_peek(f->read_buffer);
  if (v == '+' || v == '-') {
    c[position++] = (char)v;
    __pasko_buffer_char_skip(f->read_buffer);
    v = __pasko_buffer_char_peek(f->read_buffer);
  }

  if ('0' <= v && v <= '9') {
    do {
      c[position++] = (char)v;
      if (position == MAX_LITERAL_LENGTH)
        __pasko_runtime_error("float literal is too long");
      __pasko_buffer_char_skip(f->read_buffer);
      v = __pasko_buffer_char_peek(f->read_buffer);
    } while ('0' <= v && v <= '9');

    bool needs_e = true;
    if (v == '.') {
      c[position++] = (char)v;
      if (position == MAX_LITERAL_LENGTH)
        __pasko_runtime_error("float literal is too long");
      needs_e = false;
      __pasko_buffer_char_skip(f->read_buffer);
      v = __pasko_buffer_char_peek(f->read_buffer);
      if ('0' <= v && v <= '9') {
        do {
          c[position++] = (char)v;
          if (position == MAX_LITERAL_LENGTH)
            __pasko_runtime_error("float literal is too long");
          __pasko_buffer_char_skip(f->read_buffer);
          v = __pasko_buffer_char_peek(f->read_buffer);
        } while ('0' <= v && v <= '9');
      } else {
        __pasko_runtime_error("incorrectly formatted real");
        return 0;
      }
    } else {
      // Let's add a . so we can use sscanf
      c[position++] = '.';
      if (position == MAX_LITERAL_LENGTH)
        __pasko_runtime_error("float literal is too long");
    }

    if (v != 'e' && needs_e) {
      __pasko_runtime_error("incorrectly formatted real");
      return 0;
    }

    if (v == 'e') {
      __pasko_buffer_char_skip(f->read_buffer);
      v = __pasko_buffer_char_peek(f->read_buffer);
      if (v == '+' || v == '-') {
        c[position++] = (char)v;
        if (position == MAX_LITERAL_LENGTH)
          __pasko_runtime_error("float literal is too long");
        __pasko_buffer_char_skip(f->read_buffer);
        v = __pasko_buffer_char_peek(f->read_buffer);
      }
      if ('0' <= v && v <= '9') {
        do {
          c[position++] = (char)v;
          if (position == MAX_LITERAL_LENGTH)
            __pasko_runtime_error("float literal is too long");
          __pasko_buffer_char_skip(f->read_buffer);
          v = __pasko_buffer_char_peek(f->read_buffer);
        } while ('0' <= v && v <= '9');
      } else {
        __pasko_runtime_error("incorrectly formatted real");
        return 0;
      }
    }

    c[position++] = '\0';

    double result;
    int matched = sscanf(c, "%lf", &result);

    if (matched != 1) {
      __pasko_runtime_error("incorrectly formatted real");
      return 0;
    }

    return result;
  }

  __pasko_runtime_error("incorrectly formatted real");
  return 0;
}

void __pasko_read_textfile_newline(pasko_file_t *f) {
  __pasko_check_read(f);
  uint32_t v = __pasko_buffer_char_peek(f->read_buffer);
  while (v != '\n' && v != UTF8_EOF) {
    __pasko_buffer_char_skip(f->read_buffer);
    v = __pasko_buffer_char_peek(f->read_buffer);
  }
  if (v == '\n') {
    __pasko_buffer_char_skip(f->read_buffer);
  }
  if (v == UTF8_EOF) {
    __pasko_runtime_error("end of file while advancing newline");
  }
}

void __pasko_buffer_var_allocate_if_needed(pasko_file_t *f, uint64_t bytes) {
  if (f->buffer_var == NULL) {
    f->buffer_var = __pasko_allocate(bytes);
  }
}

void *__pasko_buffer_var_file(pasko_file_t *f, uint64_t bytes) {
  __pasko_buffer_var_allocate_if_needed(f, bytes);
  return f->buffer_var;
}

void __pasko_get_file(pasko_file_t *f, uint64_t bytes) {
  fread(__pasko_buffer_var_file(f, bytes), bytes, 1, f->file);
}

void __pasko_put_file(pasko_file_t *f, uint64_t bytes) {
  fwrite(__pasko_buffer_var_file(f, bytes), bytes, 1, f->file);
}

void __pasko_get_textfile(pasko_file_t *f) {
  __pasko_buffer_char_skip(f->read_buffer);
}

void __pasko_put_textfile(pasko_file_t *f) {
  uint32_t value = *__pasko_buffer_var_textfile(f);
  uint32_t str[] = {value, 0};
  uint8_t *out;
  __pasko_utf32_to_utf8(str, &out);
  fprintf(f->file, "%s", (char *)out);
  __pasko_deallocate(out);
}

void __pasko_reset_file(pasko_file_t *f, uint64_t bytes) {
  if (f->file == NULL)
    __pasko_runtime_error("file is undefined");
  f->mode = PASKO_FILE_MODE_INSPECT;
  rewind(f->file);
  if (!feof(f->file)) {
    fread(__pasko_buffer_var_file(f, bytes), bytes, 1, f->file);
  }
  f->is_textfile = false;
}

void __pasko_reset_textfile(pasko_file_t *f) {
  if (f->file == NULL)
    __pasko_runtime_error("file is undefined");
  f->mode = PASKO_FILE_MODE_INSPECT;
  rewind(f->file);
  if (f->read_buffer) {
    __pasko_buffer_char_finish(f->read_buffer);
  }
  f->read_buffer =
      __pasko_buffer_char_new(__pasko_buffer_textfile_new(f->file));
  f->is_textfile = true;
}

uint32_t *__pasko_buffer_var_textfile(pasko_file_t *f) {
  if (f->mode == PASKO_FILE_MODE_INSPECT)
    return __pasko_buffer_char_peek_addr(f->read_buffer);
  else if (f->mode == PASKO_FILE_MODE_GENERATE)
    return (uint32_t *)__pasko_buffer_var_file(f, sizeof(uint32_t));
  else
    __pasko_runtime_error(
        "invalid mode for textfile when accessing its buffer variable");
}

void __pasko_rewrite_file(pasko_file_t *f) {
  if (f->file == NULL)
    __pasko_runtime_error("file is undefined");
  rewind(f->file);
  f->read_buffer = NULL;
  ftruncate(fileno(f->file), 0);
  f->mode = PASKO_FILE_MODE_GENERATE;
  f->is_textfile = false;
}

void __pasko_rewrite_textfile(pasko_file_t *f) {
  if (f->read_buffer) {
    __pasko_buffer_char_finish(f->read_buffer);
  }
  __pasko_rewrite_file(f);
  f->is_textfile = true;
}

pasko_file_t *__pasko_get_input(void) { return &__pasko_file_input; }

pasko_file_t *__pasko_get_output(void) { return &__pasko_file_output; }

uint8_t __pasko_eof_file(pasko_file_t *f) { return feof(f->file) != 0; }

uint8_t __pasko_eof_textfile(pasko_file_t *f) {
  uint32_t v = __pasko_buffer_char_peek(f->read_buffer);
  return v == UTF8_EOF;
}

uint8_t __pasko_eoln_textfile(pasko_file_t *f) {
  uint32_t v = __pasko_buffer_char_peek(f->read_buffer);
  return v == '\n';
}

void __pasko_init_io(int argc, char *argv[], int num_program_params,
                     char *program_params[], int num_global_files,
                     pasko_file_t **global_files[]) {
  if (num_program_params != num_global_files) {
    __pasko_runtime_error(
        "inconsistent number of program parameters and global files");
  }

  // Required files.
  // We always make them available regardless of the program
  // actually using them.
  __pasko_file_output.file = stdout;
  __pasko_file_output.read_buffer = NULL;
  __pasko_file_output.mode = PASKO_FILE_MODE_GENERATE;

  __pasko_file_input.file = stdin;
  __pasko_file_input.read_buffer =
      __pasko_buffer_char_new(__pasko_buffer_textfile_new(stdin));
  __pasko_file_input.mode = PASKO_FILE_MODE_INSPECT;

  bool file_is_open[64] = {0};
  if (num_global_files > 64) {
    __pasko_runtime_error("too many files");
  }

  // Parse arguments.
  for (int i = 1; i < argc; i++) {
    char *equal = strchr(argv[i], '=');
    if (strncmp(argv[i], "--", 2) != 0) {
      __pasko_ignoring_argument(argv[i], "does not start with --");
      continue;
    }
    if (argv[i][2] == '\0') {
      __pasko_ignoring_argument(argv[i], "nothing after --");
      continue;
    }
    if (!equal) {
      __pasko_ignoring_argument(argv[i], "missing equals sign (=)");
      continue;
    }
    if (equal == &argv[i][0]) {
      __pasko_ignoring_argument(argv[i], "expecting value before the sign (=)");
      continue;
    }
    if (*(equal + 1) == '\0') {
      __pasko_ignoring_argument(argv[i], "expecting value after the sign (=)");
      continue;
    }

    ptrdiff_t colon_idx = equal - &argv[i][0];
    char *argument = __pasko_strdup(argv[i]);
    argument[colon_idx] = '\0';
    char *argument_name = argument + 2;
    char *argument_value = argument + (colon_idx + 1);

    // Now check name appears in the global names.
    // Sadly we made them to be UTF-32 encoded characters, so we better pass the
    // to utf8 first.
    bool found = false;
    int program_param_idx;
    for (program_param_idx = 0; program_param_idx < num_program_params;
         program_param_idx++) {
      char *program_param_str;
      __pasko_utf32_to_utf8((uint32_t *)program_params[program_param_idx],
                            (uint8_t **)&program_param_str);

      // In all honesty we should use UTF-8 aware routines here.
      if (strcmp(argument_name, program_param_str) == 0) {
        found = true;

        // We found the parameter. The compiler emits the global file pointer in
        // the same order so we can reuse the index.
        pasko_file_t **addr_to_var = global_files[program_param_idx];
        if (file_is_open[program_param_idx]) {
          __pasko_ignoring_argument(argv[i], "file has already been bound");
          __pasko_deallocate(program_param_str);
          continue;
        }
        int fd = open(argument_value, O_RDWR | O_CREAT, 0644);
        FILE *f = NULL;
        if (fd >= 0)
          f = fdopen(fd, "r+");
        if (fd < 0 || f == NULL) {
          char msg[256];
          snprintf(msg, 255,
                   "cannot bind file variable '%s' to external file '%s'. "
                   "Reason: %s",
                   argument_name, argument_value, strerror(errno));
          msg[255] = '\0';
          __pasko_runtime_error(msg);
        }

        pasko_file_t *new_file = __pasko_allocate(sizeof(*new_file));
        new_file->file = f;
        new_file->read_buffer = NULL;
        new_file->mode = PASKO_FILE_MODE_NONE;
        new_file->is_textfile = 0;

        *addr_to_var = new_file;

        // So we know this file has already been bound.
        file_is_open[program_param_idx] = true;
      }

      __pasko_deallocate(program_param_str);
    }
    if (!found) {
      char msg[256];
      snprintf(msg, 255, "there is no parameter program named '%s'",
               argument_name);
      msg[255] = '\0';
      __pasko_ignoring_argument(argv[i], msg);
      __pasko_deallocate(argument);
      continue;
    }

    __pasko_deallocate(argument);
  }

  // Now check all the files have been bound.
  for (int i = 0; i < num_global_files; i++) {
    if (!file_is_open[i]) {
      char *program_param_str;
      __pasko_utf32_to_utf8((uint32_t *)program_params[i],
                            (uint8_t **)&program_param_str);
      char msg[256];
      snprintf(msg, 255, "file '%s' has not been bound to an external file",
               program_param_str);
      msg[255] = '\0';
      __pasko_runtime_error(msg);
    }
  }
}

void __pasko_finish_io(int num_global_files, pasko_file_t **global_files[]) {
  __pasko_buffer_char_finish(__pasko_file_input.read_buffer);

  for (int i = 0; i < num_global_files; i++) {
    pasko_file_t *f = *global_files[i];
    if (f->is_textfile && f->mode == PASKO_FILE_MODE_INSPECT) {
      if (f->read_buffer)
        __pasko_buffer_char_finish(f->read_buffer);
    } else {
      if (f->buffer_var)
        __pasko_deallocate(f->buffer_var);
    }
    fclose(f->file);
    __pasko_deallocate(f);
  }
}
