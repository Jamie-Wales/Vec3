#pragma once
#include <stdint.h>

typedef enum {
    NUMBER_INTEGER,
    NUMBER_FLOAT,
    NUMBER_RATIONAL,
    NUMBER_COMPLEX
} NumberType;

typedef struct {
    int64_t num;
    int64_t denom;
} Rational;

typedef struct {
    double real;
    double imag;
} Complex;

typedef struct {
    NumberType type;
    union {
        int64_t integer;
        double float_val;
        Rational rational;
        Complex complex;
    } as;
} Number;

void number_print(const Number* number);
Number number_from_int(int64_t value);
Number number_from_float(double value);
Number number_from_rational(int64_t num, int64_t denom);
Number number_from_complex(double real, double imag);
