
#include "value.h"
#include <stdio.h>

void vec3_incref(Vec3Value* value)
{
    if (value != NULL) {
        value->object.ref_count++;
    }
}

void vec3_decref(Vec3Value* value)
{
    if (value != NULL) {
        assert(value->object.ref_count > 0);
        value->object.ref_count--;
        if (value->object.ref_count == 0) {
            if (value->object.destructor) {
                value->object.destructor((Vec3Object*)value);
            }
            printf("Ref count is 0 deallocating %p\n", value);
            free(value);
        }
    }
}

Vec3Value* vec3_create_integer(int64_t value)
{
    Vec3Value* num = malloc(sizeof(Vec3Value));
    num->object.type = TYPE_NUMBER;
    num->object.ref_count = 1;
    // numbers don't need a destructor decref will free this
    num->object.destructor = NULL;
    num->number.type = NUMBER_INTEGER;
    num->number.as.integer = value;
    return num;
}
Vec3Value* vec3_add(Vec3Value* a, Vec3Value* b)
{
    vec3_incref(a);
    vec3_incref(b);
    Vec3Value* result = NULL;
    if (a->number.type == NUMBER_INTEGER && b->number.type == NUMBER_INTEGER) {
        result = vec3_create_integer(
            a->number.as.integer + b->number.as.integer);
    }
    vec3_decref(a);
    vec3_decref(b);
    return result;
}
