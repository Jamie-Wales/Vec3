#pragma once
#include "number.h"
#include <stdbool.h>
#include <stddef.h>

// Forward declarations
typedef struct Vec3Value Vec3Value;
typedef struct Vec3Object Vec3Object;
typedef struct Vec3Env Vec3Env;
typedef struct HashMap HashMap;

// Structures
struct Vec3Env {
    struct Vec3Env* enclosing;
    HashMap* values;
};

typedef enum {
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_LIST,
    TYPE_FUNCTION,
    TYPE_CLOSURE,
    TYPE_BOOL,
    TYPE_NIL
} ValueType;

typedef struct Vec3Function {
    int arity;
    char* name;
    struct Vec3Env* env;
    Vec3Value* (*fn)(Vec3Value** args);
} Vec3Function;

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
        Vec3Function* function;
    } as;
} Vec3Value;

// Memory management functions
void vec3_incref(Vec3Value* value);
void vec3_decref(Vec3Value* value);
void vec3_destroy_string(Vec3Object* object);
void vec3_destroy_function(Vec3Object* object);

// Constructors
Vec3Value* vec3_new_number(Number number);
Vec3Value* vec3_new_string(const char* chars);
Vec3Value* vec3_new_list(size_t count, ...);
Vec3Value* vec3_new_bool(bool b);
Vec3Value* vec3_new_nil(void);
Vec3Value* vec3_new_function(const char* name, int arity,
    Vec3Value* (*fn)(Vec3Value** args),
    struct Vec3Env* env);

// Function call handling
Vec3Value* vec3_call_function(Vec3Value* function, Vec3Value** args, int argCount);

// Comparison operations
Vec3Value* vec3_equal(Vec3Value** args);
Vec3Value* vec3_not_equal(Vec3Value** args);
Vec3Value* vec3_less(Vec3Value** args);
Vec3Value* vec3_less_equal(Vec3Value** args);
Vec3Value* vec3_greater(Vec3Value** args);
Vec3Value* vec3_greater_equal(Vec3Value** args);

// Logical operations
Vec3Value* vec3_and(Vec3Value** args);
Vec3Value* vec3_or(Vec3Value** args);
Vec3Value* vec3_not(Vec3Value** args);

void vec3_print_internal(const Vec3Value* value, bool nl);
Vec3Value* vec3_print(Vec3Value** args);

bool vec3_is_truthy(const Vec3Value* value);
Vec3Value* vec3_to_string(Vec3Value** args);
Vec3Value* vec3_to_number(Vec3Value** args);
Vec3Value* vec3_to_bool(Vec3Value** args);
Vec3Value* vec3_input(Vec3Value** args);
Vec3Value* vec3_error(Vec3Value* message);