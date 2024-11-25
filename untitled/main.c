
#include "number.h"
#include "value.h"
#include "vec3_math.h"
#include "vec3_list.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>


int main(void) {
    Vec3Value* c = vec3_new_list(vec3_new_number(number_from_int(1)),
vec3_new_number(number_from_int(3)),
vec3_new_number(number_from_int(2)));
    vec3_print(vec3_cons(vec3_new_number(number_from_int(4)), c));
    return 0;
}
