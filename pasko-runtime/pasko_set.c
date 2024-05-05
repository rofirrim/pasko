#include <pasko_internal.h>
#include <pasko_runtime.h>

#include <stdint.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

enum set_mode {
  SM_BITMAP,
  SM_SORTED_ARRAY,
};

enum {
  MAX_BITS = UINT64_C(256),
  MAX_UNITS = (uint64_t)(MAX_BITS / UINT64_C(64))
};

struct pasko_bitmap {
  uint64_t unit[MAX_UNITS];
};

struct pasko_sorted_array {
  uint64_t size;
  int64_t *values;
};

struct pasko_set_t {
  enum set_mode mode;
  union {
    struct pasko_bitmap bitmap;
    struct pasko_sorted_array array;
  };
};

static int compare_int64(const void *xa, const void *xb) {
  int64_t a = *(int64_t *)xa;
  int64_t b = *(int64_t *)xb;
  return a - b;
}

static void sort_set(pasko_set_t *result) {
  if (result->mode != SM_SORTED_ARRAY)
    __pasko_runtime_error("attempt to sort a set that is not a sorted array");

  if (result->array.size == 0)
    __pasko_runtime_error("attempt to sort an empty set");

  if (result->array.size == 1)
    return;

  qsort(result->array.values, result->array.size, sizeof(*result->array.values),
        compare_int64);
}

static void deduplicate_set(pasko_set_t *result) {
  if (result->mode != SM_SORTED_ARRAY)
    __pasko_runtime_error(
        "attempt to deduplicate something that is not a sorted array");

  if (result->array.size == 0)
    __pasko_runtime_error("attempt to deduplicate an empty set");

  uint64_t out = 0;
  uint64_t current = 1;
  do {
    if (result->array.values[out] != result->array.values[current]) {
      if (out + 1 != current) {
        result->array.values[out + 1] = result->array.values[current];
      }
      out++;
    }
    current++;
  } while (current != result->array.size);
  result->array.size = out + 1;
}

static void pack_to_bitmap(pasko_set_t *result) {
  if (result->mode != SM_SORTED_ARRAY)
    return;

  // We may be able to turn this array into a bitmap.
  int64_t max_value = -INT64_MAX;
  int64_t min_value = INT64_MAX;
  for (uint64_t i = 0; i < result->array.size; i++) {
    max_value = result->array.values[i] > max_value ? result->array.values[i]
                                                    : max_value;
    min_value = result->array.values[i] < min_value ? result->array.values[i]
                                                    : min_value;
  }

  if (result->array.size != 0 && (min_value < 0 || max_value >= MAX_BITS))
    return;

  int64_t *values = result->array.values;
  uint64_t size = result->array.size;
  result->mode = SM_BITMAP;
  memset(result->bitmap.unit, 0, sizeof(result->bitmap.unit));
  for (uint64_t i = 0; i < size; i++) {
    uint64_t unit = values[i] / UINT64_C(64);
    uint64_t bit = values[i] % UINT64_C(64);
    result->bitmap.unit[unit] |= (UINT64_C(1) << bit);
  }
  __pasko_deallocate(values);
}

pasko_set_t *__pasko_set_new(uint64_t N, int64_t *values) {
  if ((N == 0) != (values == NULL))
    __pasko_runtime_error(
        "inconsistent number of values during creation of a set");

  bool can_use_bitmap = 1;
  for (uint64_t i = 0; i < N; i++) {
    if (values[i] < 0 || values[i] > MAX_BITS) {
      can_use_bitmap = 0;
      break;
    }
  }

  pasko_set_t *result = __pasko_allocate(sizeof(*result));
  if (can_use_bitmap) {
    result->mode = SM_BITMAP;
    memset(result->bitmap.unit, 0, sizeof(result->bitmap.unit));
    for (uint64_t i = 0; i < N; i++) {
      uint64_t unit = values[i] / UINT64_C(64);
      uint64_t bit = values[i] % UINT64_C(64);
      result->bitmap.unit[unit] |= (UINT64_C(1) << bit);
    }
  } else {
    result->mode = SM_SORTED_ARRAY;
    result->array.size = N;
    result->array.values =
        __pasko_allocate(sizeof(*result->array.values) * result->array.size);
    memcpy(result->array.values, values,
           result->array.size * sizeof(*result->array.values));
    if (result->array.size > 1) {
      sort_set(result);
      deduplicate_set(result);
    }
  }
  return result;
}

void __pasko_set_dispose(pasko_set_t *s) {
  // Do nothing.
  if (!s)
    return;

  switch (s->mode) {
  case SM_BITMAP:
    // Nothing to do.
    break;
  case SM_SORTED_ARRAY:
    __pasko_deallocate(s->array.values);
    break;
  default:
    __pasko_runtime_error("invalid set mode");
  }
}

static uint64_t safe_popcount(uint64_t t) {
  if (t == 0)
    return 0;
  return __builtin_popcount(t);
}

pasko_set_t *__pasko_set_union(pasko_set_t *a, pasko_set_t *b) {
  if (!a)
    __pasko_runtime_error("left-hand side of union is an unitialised set");
  if (!b)
    __pasko_runtime_error("right-hand side of union is an unitialised set");

  pasko_set_t *result = __pasko_allocate(sizeof(*result));
  if (a->mode == SM_BITMAP && b->mode == SM_BITMAP) {
    result->mode = SM_BITMAP;
    for (unsigned i = 0; i < MAX_UNITS; i++) {
      result->bitmap.unit[i] = a->bitmap.unit[i] | b->bitmap.unit[i];
    }
  } else {
    result->mode = SM_SORTED_ARRAY;
    if (a->mode == SM_SORTED_ARRAY && b->mode == SM_SORTED_ARRAY) {
      // If both are sorted arrays, this is easy.
      result->mode = SM_SORTED_ARRAY;
      result->array.size = a->array.size + b->array.size;
      result->array.values =
          __pasko_allocate(sizeof(*result->array.values) * result->array.size);
      // Merge
      uint64_t i = 0;
      for (uint64_t ia = 0, ib = 0; ia < a->array.size || ib < b->array.size;
           i++) {
        if (ib == b->array.size || a->array.values[ia] < b->array.values[ib]) {
          result->array.values[i] = a->array.values[ia];
          ia++;
        } else if (ia == a->array.size ||
                   a->array.values[ia] > b->array.values[ib]) {
          result->array.values[i] = a->array.values[ib];
          ib++;
        } else if (ia < a->array.size && ib < b->array.size &&
                   a->array.values[ia] == b->array.values[ib]) {
          // Inputs must be deduplicated so advancing just once should do.
          result->array.values[i] = a->array.values[ia];
          ia++;
          ib++;
        } else {
          __pasko_runtime_error("invalid element found during union");
        }
      }
      result->array.size = i;
    } else if ((a->mode == SM_BITMAP && b->mode == SM_SORTED_ARRAY) ||
               (a->mode == SM_SORTED_ARRAY && b->mode == SM_BITMAP)) {
      // FIXME: This case is handled in a bit of a lazy way.
      // Make 'a' always be the array.
      if (a->mode == SM_BITMAP) {
        pasko_set_t *tmp = a;
        a = b;
        b = tmp;
      }
      result->mode = SM_SORTED_ARRAY;
      result->array.size = a->array.size;
      uint64_t values_in_bitmap = 0;
      for (uint64_t i = 0; i < MAX_UNITS; i++) {
        values_in_bitmap += safe_popcount(b->bitmap.unit[i]);
      }
      result->array.size += values_in_bitmap;
      result->array.values =
          __pasko_allocate(sizeof(*result->array.values) * result->array.size);
      memcpy(result->array.values, a->array.values,
             sizeof(*a->array.values) * a->array.size);
      if (values_in_bitmap > 0) {
        uint64_t last = a->array.size;
        for (uint64_t i = 0; i < MAX_UNITS; i++) {
          int pc = safe_popcount(b->bitmap.unit[i]);
          if (pc == 0)
            continue;
          uint64_t base = UINT64_C(64) * i;
          for (uint64_t bit = 0; bit < UINT64_C(64); bit++) {
            if (b->bitmap.unit[i] & (UINT64_C(1) << bit)) {
              uint64_t value = base + bit;
              result->array.values[last] = value;
              last++;
            }
          }
        }
        result->array.size = last;
        if (result->array.size > 1) {
          sort_set(result);
          deduplicate_set(result);
        }
      }
    } else {
      __pasko_runtime_error("unexpected mode of sets during union");
    }
  }

  return result;
}

pasko_set_t *__pasko_set_intersection(pasko_set_t *a, pasko_set_t *b) {
  if (!a)
    __pasko_runtime_error(
        "left-hand side of intersection is an unitialised set");
  if (!b)
    __pasko_runtime_error(
        "right-hand side of intersection is an unitialised set");

  pasko_set_t *result = __pasko_allocate(sizeof(*result));

  if ((a->mode == SM_BITMAP && b->mode == SM_BITMAP) ||
      (a->mode == SM_BITMAP && b->mode == SM_SORTED_ARRAY) ||
      (a->mode == SM_SORTED_ARRAY && b->mode == SM_BITMAP)) {
    result->mode = SM_BITMAP;
    if (a->mode == SM_BITMAP && b->mode == SM_BITMAP) {
      // Easy case.
      for (uint64_t i = 0; i < MAX_UNITS; i++) {
        result->bitmap.unit[i] = a->bitmap.unit[i] & b->bitmap.unit[i];
      }
    } else {
      if (a->mode == SM_BITMAP) {
        // Make 'a' always be the array.
        pasko_set_t *tmp = a;
        a = b;
        b = tmp;
      }
      uint64_t ia = 0;
      for (uint64_t i = 0; i < MAX_UNITS && ia < a->array.size; i++) {
        result->bitmap.unit[i] = 0;
        uint64_t base = UINT64_C(64) * i;
        for (uint64_t bit = 0; bit < UINT64_C(64) && ia < a->array.size;
             bit++) {
          if (b->bitmap.unit[i] & (UINT64_C(1) << bit)) {
            int64_t value = base + bit;
            while (ia < a->array.size && a->array.values[ia] < value) {
              ia++;
            }
            if (ia < a->array.size && a->array.values[ia] == value) {
              result->bitmap.unit[i] |= (UINT64_C(1) << bit);
            }
          }
        }
      }
    }
  } else if (a->mode == SM_SORTED_ARRAY && b->mode == SM_SORTED_ARRAY) {
    // Both are arrays.
    result->mode = SM_SORTED_ARRAY;
    result->array.size = a->array.size + b->array.size;
    result->array.values =
        __pasko_allocate(sizeof(*result->array.values) * result->array.size);
    uint64_t i = 0;
    for (uint64_t ia = 0, ib = 0; ia < a->array.size && ib < b->array.size;) {
      if (a->array.values[ia] < b->array.values[ib]) {
        ia++;
      } else if (b->array.values[ib] < a->array.values[ia]) {
        ib++;
      } else {
        result->array.values[i] = a->array.values[ia];
        i++;
      }
    }
    result->array.size = i;

    pack_to_bitmap(result);
  } else {
    __pasko_runtime_error("unexpected mode of sets during intersection");
  }

  return result;
}

pasko_set_t *__pasko_set_difference(pasko_set_t *a, pasko_set_t *b) {
  if (!a)
    __pasko_runtime_error("left-hand side of difference is an unitialised set");
  if (!b)
    __pasko_runtime_error(
        "right-hand side of difference is an unitialised set");

  pasko_set_t *result = __pasko_allocate(sizeof(*result));
  if (a->mode == SM_BITMAP && b->mode == SM_BITMAP) {
    result->mode = SM_BITMAP;
    for (uint64_t i = 0; i < MAX_UNITS; i++) {
      result->bitmap.unit[i] = a->bitmap.unit[i] & ~(b->bitmap.unit[i]);
    }
  } else if (a->mode == SM_SORTED_ARRAY && b->mode == SM_SORTED_ARRAY) {
    result->mode = SM_SORTED_ARRAY;
    uint64_t i = 0;
    result->array.size = a->array.size;
    result->array.values =
        __pasko_allocate(sizeof(*result->array.values) * result->array.size);
    for (uint64_t ia = 0, ib = 0; ia < a->array.size;) {
      if (ib < b->array.size) {
        if (b->array.values[ib] < a->array.values[ia]) {
          ib++;
        } else if (a->array.values[ia] < b->array.values[ib]) {
          result->array.values[i] = a->array.values[ia];
          i++;
          ia++;
        } else {
          ia++;
          ib++;
        }
      } else {
        result->array.values[i] = a->array.values[ia];
        i++;
        ia++;
      }
    }
    result->array.size = i;
  } else if (a->mode == SM_BITMAP && b->mode == SM_SORTED_ARRAY) {
    result->mode = SM_BITMAP;
    memset(result->bitmap.unit, 0, sizeof(result->bitmap.unit));
    uint64_t ib = 0;
    for (uint64_t i = 0; i < MAX_UNITS; i++) {
      uint64_t base = i * UINT64_C(64);
      for (uint64_t bit = 0; bit < UINT64_C(64); bit++) {
        if (a->bitmap.unit[i] & (UINT64_C(1) << bit)) {
          int64_t value = base + bit;
          while (ib < b->array.size && b->array.values[ib] < value) {
            ib++;
          }
          if (ib < b->array.size && b->array.values[ib] == value) {
            // Skip value.
          } else {
            result->bitmap.unit[i] |= (UINT64_C(1) << bit);
          }
        }
      }
    }
  } else if (a->mode == SM_SORTED_ARRAY && b->mode == SM_BITMAP) {
    result->mode = SM_SORTED_ARRAY;
    result->array.size = a->array.size;
    result->array.values =
        __pasko_allocate(sizeof(*result->array.values) * result->array.size);
    uint64_t i = 0;
    for (uint64_t ia = 0; ia < a->array.size; ia++) {
      if (a->array.values[ia] < MAX_BITS) {
        uint64_t unit = a->array.values[ia] / UINT64_C(64);
        uint64_t bit = a->array.values[ia] % UINT64_C(64);
        if (!(b->bitmap.unit[unit] & (UINT64_C(1) << bit))) {
          result->array.values[i] = a->array.values[ia];
          i++;
        }
      } else {
        result->array.values[i] = a->array.values[ia];
        i++;
      }
    }
    result->array.size = i;
  } else {
    __pasko_runtime_error("unexpected mode of sets during difference");
  }

  pack_to_bitmap(result);

  return result;
}

uint8_t __pasko_set_contains(pasko_set_t *a, int64_t value) {
  if (!a)
    __pasko_runtime_error("checking membership on an uninitialised set");

  switch (a->mode) {
  case SM_BITMAP: {
    if (value >= MAX_BITS)
      return 0;

    uint64_t unit = value / UINT64_C(64);
    uint64_t bit = value % UINT64_C(64);
    return !!(a->bitmap.unit[unit] & (UINT64_C(1) << bit));
  }
  case SM_SORTED_ARRAY: {
    void *p = bsearch(&value, a->array.values, a->array.size,
                      sizeof(*a->array.values), compare_int64);
    return p != NULL;
  }
  default:
    __pasko_runtime_error("unexpected set mode when checking membership");
  }
  return 0;
}

PASKO_RUNTIME_PUBLIC pasko_set_t *__pasko_set_copy(pasko_set_t *src) {
  if (!src)
    __pasko_runtime_error("attempting to copy an uninitialised set");

  pasko_set_t *result = __pasko_allocate(sizeof(*result));
  switch (src->mode) {
  case SM_BITMAP: {
    result->mode = SM_BITMAP;
    memcpy(&result->bitmap, &src->bitmap, sizeof(src->bitmap));
    break;
  }
  case SM_SORTED_ARRAY: {
    result->mode = SM_SORTED_ARRAY;
    result->array.size = src->array.size;
    result->array.values =
        __pasko_allocate(sizeof(*result->array.values) * src->array.size);
    memcpy(result->array.values, src->array.values,
           sizeof(*result->array.values) * src->array.size);
    break;
  }
  default:
    __pasko_runtime_error("unexpected set mode during copy");
  }
  return result;
}

PASKO_RUNTIME_PUBLIC uint8_t __pasko_set_equal(pasko_set_t *a, pasko_set_t *b) {
  if (!a)
    __pasko_runtime_error(
        "left-hand side of equivalence is an unitialised set");
  if (!b)
    __pasko_runtime_error(
        "right-hand side of equivalence is an unitialised set");

  if (a == b)
    return 1;

  if (a->mode == SM_BITMAP && b->mode == SM_BITMAP) {
    for (uint64_t i = 0; i < MAX_UNITS; i++) {
      if (a->bitmap.unit[i] != b->bitmap.unit[i])
        return 0;
    }
    return 1;
  } else if (a->mode == SM_SORTED_ARRAY && b->mode == SM_SORTED_ARRAY) {
    if (a->array.size != b->array.size)
      return 0;
    for (uint64_t ia = 0; ia < a->array.size; ia++) {
      if (a->array.values[ia] != b->array.values[ia])
        return 0;
    }
    return 1;
  } else {
    // This is a strong property that we should ensure holds all the time.
    return 0;
  }
}

PASKO_RUNTIME_PUBLIC uint8_t __pasko_set_not_equal(pasko_set_t *a,
                                                   pasko_set_t *b) {
  return !__pasko_set_equal(a, b);
}

// a is subset of b
PASKO_RUNTIME_PUBLIC uint8_t __pasko_set_is_subset(pasko_set_t *a,
                                                   pasko_set_t *b) {
  if (!a)
    __pasko_runtime_error("left-hand side of subset is an unitialised set");
  if (!b)
    __pasko_runtime_error("right-hand side of subset is an unitialised set");

  if (a == b)
    return 1;

  if (a->mode == SM_SORTED_ARRAY && b->mode == SM_BITMAP) {
    // If 'a' could ever be a subset of 'b', then it should be possible to
    // represent 'a' using a bitmap to.  So if we determined a sorted array was
    // needed, it means we need something larger that cannot be a subset.
    return 0;
  }

  if (a->mode == SM_SORTED_ARRAY && b->mode == SM_SORTED_ARRAY) {
    if (a->array.size > b->array.size) {
      // In this case there will always be at least one element in 'a' that will
      // not be matched with an element of 'b'. So, it cannot be a subset.
      return 0;
    }
    for (uint64_t ia = 0, ib = 0; ia < a->array.size;) {
      if (ib < b->array.size && b->array.values[ib] < a->array.values[ia]) {
        ib++;
      } else if (ib < b->array.size &&
                 b->array.values[ib] == a->array.values[ia]) {
        ia++;
        ib++;
      } else if (ib >= b->array.size ||
                 a->array.values[ia] < b->array.values[ib]) {
        // We did not find one value of 'a' in 'b'
        return 0;
      }
    }
    return 1;
  } else if (a->mode == SM_BITMAP && b->mode == SM_BITMAP) {
    for (uint64_t i = 0; i < MAX_UNITS; i++) {
      if ((a->bitmap.unit[i] & b->bitmap.unit[i]) != a->bitmap.unit[i])
        return 0;
    }
    return 1;
  } else if (a->mode == SM_BITMAP && b->mode == SM_SORTED_ARRAY) {
    for (uint64_t unit = 0, ib = 0; unit < MAX_UNITS; unit++) {
      uint64_t base = unit * UINT64_C(64);
      for (uint64_t bit = 0; bit < UINT64_C(64); bit++) {
        if (a->bitmap.unit[unit] & (UINT64_C(1) << bit)) {
          int64_t value = base + bit;
          while (ib < b->array.size && b->array.values[ib] < value) {
            ib++;
          }
          if (ib < b->array.size && b->array.values[ib] == value) {
            ib++;
          } else {
            // ib >= b->array.size || b->array.values[ib] != value
            // We asserted above, in leaving the loop, that if ib <
            // b->array.size then b->array.values[ib] >= value Because it cannot
            // be the same, then ib >= b->array.size || b->array.values[ib] >
            // value Which means this value is not in the sorted array.
            return 0;
          }
        }
      }
    }
    return 1;
  }
  __pasko_runtime_error("unreachable case");
}