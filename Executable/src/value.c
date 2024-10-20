//
// Created by Jamie Wales on 20/10/2024.
//

#include "value.h"

Value add(double a, double b) {
    Value v = NUMBER_VAL(a + b);
    return v;
}

