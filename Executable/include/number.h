#pragma once

#include <stdint.h>
typedef enum {
    NUMBER_INTEGER,
    NUMBER_FLOAT,
    NUMBER_RATIONAL,
    NUMBER_COMPLEX
} NumberType;

typedef struct {
    NumberType type;
    union {
        int64_t integer;
        double float_val;
        struct {
            int64_t num;
            int64_t denom;
        } rational;
        struct {
            double real;
            double imag;
        } complex;
    } as;
} Number;
