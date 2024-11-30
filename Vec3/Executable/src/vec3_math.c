#include "vec3_math.h"
Vec3Value* vec3_add(Vec3Value* a, Vec3Value* b)
{
    vec3_incref(a);
    vec3_incref(b);

    Vec3Value* result = NULL;

    if (a->object.type == TYPE_NUMBER && b->object.type == TYPE_NUMBER) {
        Number result_num;
        switch (a->as.number.type) {
        case NUMBER_INTEGER:
            if (b->as.number.type == NUMBER_INTEGER) {
                result_num = number_from_int(
                    a->as.number.as.integer + b->as.number.as.integer);
                result = vec3_new_number(result_num);
            }
            break;
        default:
            break;
        }
    }

    vec3_decref(a);
    vec3_decref(b);

    return result;
}

Vec3Value* vec3_multiply(Vec3Value* a, Vec3Value* b)
{
    vec3_incref(a);
    vec3_incref(b);

    Vec3Value* result = NULL;

    if (a->object.type == TYPE_NUMBER && b->object.type == TYPE_NUMBER) {
        switch (a->as.number.type) {
        case NUMBER_INTEGER:
            if (b->as.number.type == NUMBER_INTEGER) {
                result = vec3_new_number(number_from_int(
                    a->as.number.as.integer * b->as.number.as.integer));
            }
            break;
        case NUMBER_RATIONAL:
            if (b->as.number.type == NUMBER_RATIONAL) {
                result = vec3_new_number(number_from_rational(
                    a->as.number.as.rational.num * b->as.number.as.rational.num,
                    a->as.number.as.rational.denom * b->as.number.as.rational.denom));
            }
            break;
        case NUMBER_COMPLEX:
            if (b->as.number.type == NUMBER_COMPLEX) {
                double ar = a->as.number.as.complex.real;
                double ai = a->as.number.as.complex.imag;
                double br = b->as.number.as.complex.real;
                double bi = b->as.number.as.complex.imag;
                result = vec3_new_number(number_from_complex(
                    ar * br - ai * bi,
                    ar * bi + ai * br));
            }
            break;
            // Add other number type combinations...
        case NUMBER_FLOAT:
            break;
        }
        /* } else if (a->object.type == TYPE_LIST && b->object.type == TYPE_LIST) { */
        /*     if (a->as.list->length == b->as.list->length) { */
        /*         result = vec3_new_list(a->as.list->length); */
        /*         for (size_t i = 0; i < a->as.list.length; i++) { */
        /*             Vec3Value* item_result = vec3_multiply(a->as.list.items[i], b->as.list.items[i]); */
        /*             vec3_list_append(result, item_result); */
        /*             vec3_decref(item_result); */
        /*         } */
        /*     } */
    }

    vec3_decref(a);
    vec3_decref(b);
    return result ? result : vec3_new_nil();
}
