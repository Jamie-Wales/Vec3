#include "value.h"
#include <stdio.h>
int main(void)
{
    Vec3Value* x = vec3_create_integer(5);
    Vec3Value* y = vec3_create_integer(10);
    Vec3Value* sum = vec3_add(x, y);
    vec3_decref(x);
    vec3_decref(y);
    vec3_decref(sum);
}
