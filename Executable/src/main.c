#include "value.h"
#include <number.h>
#include <stdio.h>

int main(void)
{
    Vec3Value* x = vec3_new_number(number_from_int(10));
    Vec3Value* y = vec3_new_number(number_from_int(20));
    Vec3Value* sum = vec3_add(x, y);
    vec3_print(sum);
    vec3_decref(x);
    vec3_decref(y);
    vec3_decref(sum);
}
