#include <pasko_internal.h>
#include <pasko_runtime.h>

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

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
  pasko_buffer_textfile_t *b = malloc(sizeof(*b));
  __pasko_buffer_textfile_init(b, f);
  return b;
}

static void __pasko_buffer_textfile_finish(pasko_buffer_textfile_t *b) {
  free(b->buffer);
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
      void *r = realloc(b->buffer, new_size * sizeof(int));
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
  pasko_buffer_char_t *b = malloc(sizeof(*b));
  __pasko_buffer_char_init(b, src);
  return b;
}

#define UTF8_EOF UINT32_MAX
#define UNICODE_REPLACEMENT_CHAR 0xFFFD

static int __pasko_buffer_char_peek(pasko_buffer_char_t *b) {
  assert(b->end >= b->start);

  size_t num_queued_items = b->end - b->start;

  if (num_queued_items < 1) {
    assert(b->end != SIZE_MAX);
    // Check if there is enough room in the current buffer. If not reallocate.
    if (b->end + 1 > b->size) {
      size_t new_size = b->size ? b->size * 2 : INITIAL_UTF8_BUFFER_SIZE;
      void *r = realloc(b->buffer, new_size * sizeof(uint32_t));
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
    } else if (b0 < 0x80) {
      // 1 byte.
      b->buffer[b->end] = (unsigned char)b0;
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
    } else {
      b->buffer[b->end] = UNICODE_REPLACEMENT_CHAR;
    }
    b->end++;
  }

  return b->buffer[b->start];
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
  free(b->buffer);
}

// File
typedef enum pasko_file_mode_t {
  PASKO_FILE_MODE_NONE,
  PASKO_FILE_MODE_INSPECT,
  PASKO_FILE_MODE_GENERATE,
} pasko_file_mode_t;

struct pasko_file_t {
  FILE *file;
  pasko_buffer_char_t *read_buffer;
  pasko_file_mode_t mode : 2;
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
  uint64_t tmp_result = 0;
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
int64_t __pasko_read_i64(void) {
  return __pasko_read_textfile_i64(&__pasko_file_input);
}

double __pasko_read_textfile_f64(pasko_file_t *f) {
  enum { MAX_LITERAL_LENGTH = 128 };
  char c[MAX_LITERAL_LENGTH];
  int position = 0;

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
        __pasko_runtime_error("literal is too long");
      __pasko_buffer_char_skip(f->read_buffer);
      v = __pasko_buffer_char_peek(f->read_buffer);
    } while ('0' <= v && v <= '9');

    bool needs_e = true;
    if (v == '.') {
      needs_e = false;
      __pasko_buffer_char_skip(f->read_buffer);
      v = __pasko_buffer_char_peek(f->read_buffer);
      if ('0' <= v && v <= '9') {
        do {
          c[position++] = (char)v;
          if (position == MAX_LITERAL_LENGTH)
            __pasko_runtime_error("literal is too long");
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
        __pasko_runtime_error("literal is too long");
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
          __pasko_runtime_error("literal is too long");
        __pasko_buffer_char_skip(f->read_buffer);
        v = __pasko_buffer_char_peek(f->read_buffer);
      }
      if ('0' <= v && v <= '9') {
        do {
          c[position++] = (char)v;
          if (position == MAX_LITERAL_LENGTH)
            __pasko_runtime_error("literal is too long");
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
double __pasko_read_f64(void) {
  return __pasko_read_textfile_f64(&__pasko_file_input);
}

void __pasko_read_textfile_newline(pasko_file_t *f) {
  uint32_t v = __pasko_buffer_char_peek(f->read_buffer);
  while (v != '\n' && v != UTF8_EOF) {
    __pasko_buffer_char_skip(f->read_buffer);
    v = __pasko_buffer_char_peek(f->read_buffer);
  }
  if (v == UTF8_EOF) {
    __pasko_runtime_error("end of file while advancing newline");
  }
}
void __pasko_read_newline(void) {
  __pasko_read_textfile_newline(&__pasko_file_input);
}

void __pasko_init_io(int argc, char *argv[]) {
  (void)argc;
  (void)argv;

  __pasko_file_output.file = stdout;
  __pasko_file_output.read_buffer = NULL;
  __pasko_file_output.mode = PASKO_FILE_MODE_GENERATE;

  __pasko_file_input.file = stdin;
  __pasko_file_input.read_buffer =
      __pasko_buffer_char_new(__pasko_buffer_textfile_new(stdin));
  __pasko_file_input.mode = PASKO_FILE_MODE_INSPECT;
}

void __pasko_finish_io(void) {
  __pasko_buffer_char_finish(__pasko_file_input.read_buffer);
}
