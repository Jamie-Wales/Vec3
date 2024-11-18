#pragma once
#include "number.h"
#include <stdbool.h>
#include <stddef.h>

typedef struct Vec3Value Vec3Value;
typedef struct Vec3Object Vec3Object;

typedef enum {
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_LIST,
    TYPE_FUNCTION,
    TYPE_CLOSURE,
    TYPE_NIL
} ValueType;

typedef struct Vec3Object {
    ValueType type;
    size_t ref_count;
    void (*destructor)(struct Vec3Object*);
} Vec3Object;

typedef struct {
    char* chars;
    size_t length;
} String;

typedef struct {
    Vec3Value** items;
    size_t length;
    size_t capacity;
} List;

typedef struct Vec3Value {
    Vec3Object object;
    union {
        Number number;
        String string;
        List list;
    } as;
} Vec3Value;

void vec3_incref(Vec3Value* value);
void vec3_decref(Vec3Value* value);

Vec3Value* vec3_new_number(Number number);
Vec3Value* vec3_new_string(const char* chars);
Vec3Value* vec3_new_list(size_t initial_capacity);
Vec3Value* vec3_new_nil(void);

void vec3_destroy_string(Vec3Object* object);
void vec3_destroy_list(Vec3Object* object);

Vec3Value* vec3_add(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_subtract(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_multiply(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_divide(Vec3Value* a, Vec3Value* b);

void vec3_list_append(Vec3Value* list, Vec3Value* value);
Vec3Value* vec3_list_get(Vec3Value* list, size_t index);
void vec3_list_set(Vec3Value* list, size_t index, Vec3Value* value);

void vec3_print(const Vec3Value* value);
bool vec3_is_truthy(const Vec3Value* value);
