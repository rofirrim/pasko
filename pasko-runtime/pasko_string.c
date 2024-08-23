#include <pasko_internal.h>
#include <pasko_runtime.h>
#include <stdint.h>

void __pasko_utf32_to_utf8_n(const uint32_t *input_buffer,
                             uint8_t **ref_out_buffer, const uint64_t num_chars) {
  if (!input_buffer) {
    __pasko_runtime_error("when converting UTF-32 to UTF-8: null input buffer");
  }
  if (!ref_out_buffer) {
    __pasko_runtime_error(
        "when converting UTF-32 to UTF-8: null output buffer reference");
  }

  // Count bytes.
  uint64_t remaining_chars = num_chars;
  size_t bytes_out = 1;
  {
    const uint32_t *p = input_buffer;
    while (remaining_chars > 0) {
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
      remaining_chars--;
    }
  }

  uint8_t *out_buffer = __pasko_allocate(bytes_out);

  remaining_chars = num_chars;
  const uint32_t *p = input_buffer;
  uint8_t *q = out_buffer;
  while (remaining_chars > 0) {
    uint32_t cp = *p;
    if (cp < 0x80) {
      *q = cp & 0xff;
    } else if (cp < 0x800) {
      *q = 0xc0 | (cp >> 6);
      q++;
      *q = 0x80 | (cp & 0x3f);
    } else if (cp < 0x10000) {
      *q = 0xe0 | (cp >> 12);
      q++;
      *q = 0x80 | ((cp >> 6) & 0x3f);
      q++;
      *q = 0x80 | (cp & 0x3f);
    } else if (cp < 0x200000) {
      *q = 0xf0 | (cp >> 18);
      q++;
      *q = 0x80 | ((cp >> 12) & 0x3f);
      q++;
      *q = 0x80 | ((cp >> 6) & 0x3f);
      q++;
      *q = 0x80 | (cp & 0x3f);
    } else {
      // Should not happen, but just in case.
      __pasko_runtime_error(
          "when converting UTF-32 to UTF-8: invalid codepoint during output");
    }
    p++;
    q++;
    remaining_chars--;
  }
  *q = 0;

  *ref_out_buffer = out_buffer;
}

void __pasko_utf32_to_utf8(const uint32_t *input_buffer,
                           uint8_t **ref_out_buffer) {
  uint64_t num_chars;
  const uint32_t *p = input_buffer;
  while (*p != 0) {
    p++;
  }
  num_chars = p - input_buffer;
  __pasko_utf32_to_utf8_n(input_buffer, ref_out_buffer, num_chars);
}
