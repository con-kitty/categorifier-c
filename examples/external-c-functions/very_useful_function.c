#include "very_useful_library.h"
#include <stdint.h>
#include <stdio.h>

Result VeryUsefulFunction(int64_t x, int64_t y) {
  Result r = {.sum = x + y, .diff = x - y, .avg = (x + y) / 2.0};
  return r;
}
