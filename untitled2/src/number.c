#include "number.h"
#include <stdio.h>

void number_print(const Number* number)
{
    switch (number->type) {
    case NUMBER_INTEGER:
        printf("%lld", number->as.integer);
        break;
    case NUMBER_FLOAT:
        printf("%g", number->as.float_val);
        break;
    case NUMBER_RATIONAL:
        printf("%lld/%lld", number->as.rational.num, number->as.rational.denom);
        break;
    case NUMBER_COMPLEX:
        if (number->as.complex.imag >= 0) {
            printf("%g+%gi", number->as.complex.real, number->as.complex.imag);
        } else {
            printf("%g%gi", number->as.complex.real, number->as.complex.imag);
        }
        break;
    }
}

Number number_from_int(int64_t value)
{
    Number num = { .type = NUMBER_INTEGER, .as.integer = value };
    return num;
}

Number number_from_float(double value)
{
    Number num = { .type = NUMBER_FLOAT, .as.float_val = value };
    return num;
}

Number number_from_rational(int64_t num, int64_t denom)
{
    Number number = { .type = NUMBER_RATIONAL, .as.rational = { .num = num, .denom = denom } };
    return number;
}

Number number_from_complex(double real, double imag)
{
    Number num = { .type = NUMBER_COMPLEX, .as.complex = { .real = real, .imag = imag } };
    return num;
}

double number_to_double(const Number* num) {
    switch (num->type) {
        case NUMBER_INTEGER:
            return (double)num->as.integer;
        case NUMBER_FLOAT:
            return num->as.float_val;
        case NUMBER_RATIONAL:
            return (double)num->as.rational.num / (double)num->as.rational.denom;
        case NUMBER_COMPLEX:
            return num->as.complex.real;
        default:
            return 0.0;
    }
}