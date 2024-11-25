#include "value.h"
#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
            free(value);
        }
    }
}

void vec3_destroy_string(Vec3Object* object)
{
    Vec3Value* value = (Vec3Value*)object;
    free(value->as.string.chars);
}

void vec3_destroy_list(Vec3Object* object)
{
    Vec3Value* value = (Vec3Value*)object;
    for (size_t i = 0; i < value->as.list.length; i++) {
        vec3_decref(value->as.list.items[i]);
    }
    free(value->as.list.items);
}

Vec3Value* vec3_new_number(Number number)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_NUMBER;
    value->object.ref_count = 1;
    value->object.destructor = NULL;
    value->as.number = number;
    return value;
}

Vec3Value* vec3_new_string(const char* chars)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_STRING;
    value->object.ref_count = 1;
    value->object.destructor = vec3_destroy_string;
    value->as.string.length = strlen(chars);
    value->as.string.chars = strdup(chars);
    return value;
}
Vec3Value* vec3_new_list(size_t initial_capacity)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_LIST;
    value->object.ref_count = 1;
    value->object.destructor = vec3_destroy_list;
    value->as.list.items = malloc(sizeof(Vec3Value*) * initial_capacity);
    value->as.list.length = 0;
    value->as.list.capacity = initial_capacity;
    return value;
}

Vec3Value* vec3_new_list_values(size_t count, ...)
{
    Vec3Value* list = vec3_new_list(count);
    va_list args;
    va_start(args, count);

    for (size_t i = 0; i < count; i++) {
        Vec3Value* value = va_arg(args, Vec3Value*);
        vec3_list_append(list, value);
    }

    va_end(args);
    return list;
}

Vec3Value* vec3_new_bool(bool b)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_BOOL;
    value->object.ref_count = 1;
    value->object.destructor = NULL;
    value->as.boolean = b;
    return value;
}

Vec3Value* vec3_new_nil(void)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_NIL;
    value->object.ref_count = 1;
    value->object.destructor = NULL;
    return value;
}
void vec3_list_append(Vec3Value* list, Vec3Value* value)
{
    assert(list->object.type == TYPE_LIST);

    if (list->as.list.length >= list->as.list.capacity) {
        size_t new_capacity = list->as.list.capacity * 2;
        list->as.list.items = realloc(list->as.list.items,
            sizeof(Vec3Value*) * new_capacity);
        list->as.list.capacity = new_capacity;
    }

    vec3_incref(value);
    list->as.list.items[list->as.list.length++] = value;
}

Vec3Value* vec3_list_get(Vec3Value* list, size_t index)
{
    assert(list->object.type == TYPE_LIST);

    if (index >= list->as.list.length) {
        return vec3_new_nil();
    }

    Vec3Value* value = list->as.list.items[index];
    vec3_incref(value);
    return value;
}

void vec3_list_set(Vec3Value* list, size_t index, Vec3Value* value)
{
    assert(list->object.type == TYPE_LIST);

    if (index >= list->as.list.length) {
        return;
    }

    vec3_incref(value);
    vec3_decref(list->as.list.items[index]);
    list->as.list.items[index] = value;
}

bool vec3_is_truthy(const Vec3Value* value)
{
    if (value == NULL || value->object.type == TYPE_NIL) {
        return false;
    }

    switch (value->object.type) {
    case TYPE_NUMBER:
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            return value->as.number.as.integer != 0;
        case NUMBER_FLOAT:
            return value->as.number.as.float_val != 0.0;
        case NUMBER_RATIONAL:
            return value->as.number.as.rational.num != 0;
        case NUMBER_COMPLEX:
            return value->as.number.as.complex.real != 0.0 || value->as.number.as.complex.imag != 0.0;
        }
    case TYPE_STRING:
        return value->as.string.length > 0;
    case TYPE_LIST:
        return value->as.list.length > 0;
    default:
        return true;
    }
}

void vec3_print(const Vec3Value* value)
{
    if (value == NULL) {
        printf("null");
        return;
    }

    switch (value->object.type) {
    case TYPE_NUMBER:
        number_print(&value->as.number);
        break;
    case TYPE_STRING:
        printf("\"%s\"", value->as.string.chars);
        break;
    case TYPE_LIST:
        printf("[");
        for (size_t i = 0; i < value->as.list.length; i++) {
            if (i > 0)
                printf(", ");
            vec3_print(value->as.list.items[i]);
        }
        printf("]");
        break;
    case TYPE_NIL:
        printf("nil");
        break;
    default:
        printf("<unknown>");
        break;
    }
}
Vec3Value* vec3_sqrt(Vec3Value* value)
{
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        double val = 0.0;
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            val = sqrt((double)value->as.number.as.integer);
            break;
        case NUMBER_FLOAT:
            val = sqrt(value->as.number.as.float_val);
            break;
        case NUMBER_RATIONAL:
            val = sqrt((double)value->as.number.as.rational.num / (double)value->as.number.as.rational.denom);
            break;
        case NUMBER_COMPLEX:
            // Complex sqrt not implemented yet
            break;
        }
        result = vec3_new_number(number_from_float(val));
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_abs(Vec3Value* value)
{
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            result = vec3_new_number(number_from_int(llabs(value->as.number.as.integer)));
            break;
        case NUMBER_FLOAT:
            result = vec3_new_number(number_from_float(fabs(value->as.number.as.float_val)));
            break;
        case NUMBER_RATIONAL:
            result = vec3_new_number(number_from_rational(
                llabs(value->as.number.as.rational.num),
                llabs(value->as.number.as.rational.denom)));
            break;
        case NUMBER_COMPLEX: {
            double real = value->as.number.as.complex.real;
            double imag = value->as.number.as.complex.imag;
            result = vec3_new_number(number_from_float(sqrt(real * real + imag * imag)));
            break;
        }
        }
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

Vec3Value* vec3_floor(Vec3Value* value)
{
    vec3_incref(value);
    Vec3Value* result = NULL;

    if (value->object.type == TYPE_NUMBER) {
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            result = vec3_new_number(value->as.number); // Already integral
            break;
        case NUMBER_FLOAT:
            result = vec3_new_number(number_from_float(floor(value->as.number.as.float_val)));
            break;
        case NUMBER_RATIONAL:
            result = vec3_new_number(number_from_int(
                value->as.number.as.rational.num / value->as.number.as.rational.denom));
            break;
        case NUMBER_COMPLEX:
            printf(stderr, "Not Implemeneted");
            exit(0);
            break;
        }
    }

    vec3_decref(value);
    return result ? result : vec3_new_nil();
}

// Comparison operations
Vec3Value* vec3_equal(Vec3Value* a, Vec3Value* b)
{
    vec3_incref(a);
    vec3_incref(b);

    bool result = false;

    if (a->object.type != b->object.type) {
        result = false;
    } else {
        switch (a->object.type) {
        case TYPE_NUMBER:
            if (a->as.number.type == b->as.number.type) {
                switch (a->as.number.type) {
                case NUMBER_INTEGER:
                    result = a->as.number.as.integer == b->as.number.as.integer;
                    break;
                case NUMBER_FLOAT:
                    result = a->as.number.as.float_val == b->as.number.as.float_val;
                    break;
                case NUMBER_RATIONAL:
                    result = (a->as.number.as.rational.num * b->as.number.as.rational.denom == b->as.number.as.rational.num * a->as.number.as.rational.denom);
                    break;
                case NUMBER_COMPLEX:
                    result = (a->as.number.as.complex.real == b->as.number.as.complex.real && a->as.number.as.complex.imag == b->as.number.as.complex.imag);
                    break;
                }
            }
            break;
        case TYPE_STRING:
            result = strcmp(a->as.string.chars, b->as.string.chars) == 0;
            break;
        case TYPE_NIL:
            result = true;
            break;
        case TYPE_BOOL:
            result = a->as.boolean == b->as.boolean;
            break;
        default:
            printf(stderr, "Not Implemeneted");
            exit(0);
        }
    }

    vec3_decref(a);
    vec3_decref(b);
    return vec3_new_bool(result);
}

Vec3Value* vec3_less(Vec3Value* a, Vec3Value* b)
{
    vec3_incref(a);
    vec3_incref(b);

    bool result = false;

    if (a->object.type == TYPE_NUMBER && b->object.type == TYPE_NUMBER) {
        double a_val = 0.0, b_val = 0.0;

        switch (a->as.number.type) {
        case NUMBER_INTEGER:
            a_val = (double)a->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            a_val = a->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            a_val = (double)a->as.number.as.rational.num / (double)a->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            a_val = a->as.number.as.complex.real; // Compare real parts only
            break;
        }

        switch (b->as.number.type) {
        case NUMBER_INTEGER:
            b_val = (double)b->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            b_val = b->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            b_val = (double)b->as.number.as.rational.num / (double)b->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            b_val = b->as.number.as.complex.real; // Compare real parts only
            break;
        }

        result = a_val < b_val;
    } else if (a->object.type == TYPE_STRING && b->object.type == TYPE_STRING) {
        result = strcmp(a->as.string.chars, b->as.string.chars) < 0;
    }

    vec3_decref(a);
    vec3_decref(b);
    return vec3_new_bool(result);
}

// Logical operations
Vec3Value* vec3_and(Vec3Value* a, Vec3Value* b)
{
    vec3_incref(a);
    vec3_incref(b);
    bool result = vec3_is_truthy(a) && vec3_is_truthy(b);
    vec3_decref(a);
    vec3_decref(b);
    return vec3_new_bool(result);
}

Vec3Value* vec3_or(Vec3Value* a, Vec3Value* b)
{
    vec3_incref(a);
    vec3_incref(b);
    bool result = vec3_is_truthy(a) || vec3_is_truthy(b);
    vec3_decref(a);
    vec3_decref(b);
    return vec3_new_bool(result);
}

Vec3Value* vec3_not(Vec3Value* value)
{
    vec3_incref(value);
    bool result = !vec3_is_truthy(value);
    vec3_decref(value);
    return vec3_new_bool(result);
}

// List operations
Vec3Value* vec3_cons(Vec3Value* value, Vec3Value* list)
{
    if (list->object.type != TYPE_LIST) {
        return vec3_new_nil();
    }

    Vec3Value* new_list = vec3_new_list(list->as.list.length + 1);
    vec3_list_append(new_list, value);

    for (size_t i = 0; i < list->as.list.length; i++) {
        vec3_list_append(new_list, list->as.list.items[i]);
    }

    return new_list;
}

// Basic I/O
Vec3Value* vec3_input(void)
{
    char buffer[1024];
    if (fgets(buffer, sizeof(buffer), stdin)) {
        size_t len = strlen(buffer);
        if (len > 0 && buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
        }
        return vec3_new_string(buffer);
    }
    return vec3_new_nil();
}
