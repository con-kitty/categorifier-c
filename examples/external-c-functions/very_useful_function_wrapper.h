#include <stdint.h>
#include <stdio.h>

void Wrap_VeryUsefulFunction(
    // input
    const bool *input_bool, const int8_t *input_int8_t,
    const int16_t *input_int16_t, const int32_t *input_int32_t,
    const int64_t *input_int64_t, const uint8_t *input_uint8_t,
    const uint16_t *input_uint16_t, const uint32_t *input_uint32_t,
    const uint64_t *input_uint64_t, const float *input_float,
    const double *input_double,

    // output
    bool *output_bool, int8_t *output_int8_t, int16_t *output_int16_t,
    int32_t *output_int32_t, int64_t *output_int64_t, uint8_t *output_uint8_t,
    uint16_t *output_uint16_t, uint32_t *output_uint32_t,
    uint64_t *output_uint64_t, float *output_float, double *output_double);
