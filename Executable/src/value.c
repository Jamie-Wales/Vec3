#include "value.h"
#include <assert.h>
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

Vec3Value* vec3_new_nil(void)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_NIL;
    value->object.ref_count = 1;
    value->object.destructor = NULL;
    return value;
}

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
