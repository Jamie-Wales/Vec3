
#include "number.h"
#include "value.h"
#include "vec3_math.h"
#include "vec3_list.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>


int main(void) {
    Vec3Value* c = vec3_new_list(3, vec3_new_number(number_from_int(1)),
vec3_new_number(number_from_int(2)),
vec3_new_number(number_from_int(3)));;
    vec3_print(vec3_cons(vec3_new_number(number_from_int(4)), c), true);
    Vec3Value* a = vec3_new_number(number_from_int(10));
    Vec3Value* b = vec3_new_number(number_from_int(20));
    vec3_print(vec3_add(a, b), true);
    return 0;
}
