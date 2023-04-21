#include <stdint.h>

typedef struct {
  int64_t sum;
  int64_t diff;
  double avg;
} Result;

Result VeryUsefulFunction(const int64_t x, const int64_t y);
