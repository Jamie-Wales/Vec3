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
    TYPE_BOOL,
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

typedef struct vec3_list {
    Vec3Value* value;
    struct vec3_list* prev;
    struct vec3_list* next;
    size_t length;
} vec3_list;

typedef struct Vec3Value {
    Vec3Object object;
    union {
        Number number;
        String string;
        vec3_list* list;
        bool boolean;
    } as;
} Vec3Value;

void vec3_incref(Vec3Value* value);
void vec3_decref(Vec3Value* value);

Vec3Value* vec3_new_number(Number number);
Vec3Value* vec3_new_string(const char* chars);
Vec3Value* vec3_new_list(size_t count, ...);
Vec3Value* vec3_new_nil(void);

void vec3_destroy_string(Vec3Object* object);

void vec3_print(const Vec3Value* value);
bool vec3_is_truthy(const Vec3Value* value);
#define VEC3_PI 3.14159265358979323846
#define VEC3_E 2.71828182845904523536
#define VEC3_TAU 6.28318530717958647692

// Basic math operations
Vec3Value* vec3_power(Vec3Value* base, Vec3Value* exp);
Vec3Value* vec3_sqrt(Vec3Value* value);
Vec3Value* vec3_abs(Vec3Value* value);
Vec3Value* vec3_floor(Vec3Value* value);
Vec3Value* vec3_trunc(Vec3Value* value);

// Trigonometric functions
Vec3Value* vec3_cos(Vec3Value* value);
Vec3Value* vec3_sin(Vec3Value* value);
Vec3Value* vec3_tan(Vec3Value* value);
Vec3Value* vec3_acos(Vec3Value* value);
Vec3Value* vec3_asin(Vec3Value* value);
Vec3Value* vec3_atan(Vec3Value* value);

// Exponential and logarithmic
Vec3Value* vec3_exp(Vec3Value* value);
Vec3Value* vec3_log(Vec3Value* base, Vec3Value* value);
Vec3Value* vec3_log10(Vec3Value* value);

// List operations

// Comparison operations
Vec3Value* vec3_equal(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_not_equal(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_less(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_less_equal(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_greater(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_greater_equal(Vec3Value* a, Vec3Value* b);

// Logical operations
Vec3Value* vec3_and(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_or(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_not(Vec3Value* value);

// Vector operations
Vec3Value* vec3_dot_product(Vec3Value* a, Vec3Value* b);
Vec3Value* vec3_cross_product(Vec3Value* a, Vec3Value* b);

// Numerical methods
Vec3Value* vec3_newton_raphson(Vec3Value* f, Vec3Value* df, Vec3Value* init, Vec3Value* tol, Vec3Value* max_iter);
Vec3Value* vec3_bisection(Vec3Value* f, Vec3Value* a, Vec3Value* b, Vec3Value* tol, Vec3Value* max_iter);
Vec3Value* vec3_find_integral(Vec3Value* f, Vec3Value* a, Vec3Value* b);

// Type conversion
Vec3Value* vec3_to_string(Vec3Value* value);
Vec3Value* vec3_to_number(Vec3Value* value);
Vec3Value* vec3_to_bool(Vec3Value* value);

Vec3Value* vec3_input(void);
Vec3Value* vec3_error(Vec3Value* message);
