#pragma once
#include "number.h"
#include <stdbool.h>
#include <stddef.h>

#define TABLE_MAX_LOAD 0.75

// Forward declarations
struct Vec3Value;
struct Vec3Env;
struct Entry;
struct HashMap;
struct Vec3Thunk;
struct Vec3TrampolineValue;

typedef struct Vec3Value Vec3Value;
typedef struct Vec3Env Vec3Env;
typedef struct Entry Entry;
typedef struct HashMap HashMap;
typedef struct Vec3Thunk Vec3Thunk;
typedef struct Vec3TrampolineValue Vec3TrampolineValue;

// Value types enum
typedef enum {
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_RECORD,
    TYPE_LIST,
    TYPE_FUNCTION,
    TYPE_CLOSURE,
    TYPE_BOOL,
    TYPE_NIL,
    TYPE_TRAMPOLINE
} ValueType;

// Base object structure - defined before use
typedef struct Vec3Object {
    ValueType type;
    size_t ref_count;
    void (*destructor)(struct Vec3Object*);
} Vec3Object;

// HashMap related structures
struct Entry {
    char* key;
    Vec3Value* value;
    struct Entry* next;
};

struct HashMap {
    Entry** entries;
    size_t capacity;
    size_t count;
};

// String structure
typedef struct {
    char* chars;
    size_t length;
} String;

// List structure
typedef struct vec3_list {
    Vec3Value* value;
    struct vec3_list* prev;
    struct vec3_list* next;
    size_t length;
} vec3_list;

// Function structure
typedef struct {
    char* name;
    int arity;
    Vec3Value* (*fn)(Vec3Value** args);
    Vec3Env* env;
    bool is_recursive;
} Vec3Function;

typedef struct Vec3Thunk {
    Vec3Function* func;
    Vec3Value** args;
    int arg_count;
} Vec3Thunk;

typedef struct Vec3TrampolineValue {
    Vec3Object object;
    union {
        Vec3Thunk* thunk;
        Vec3Value* value;
    } as;
    bool is_thunk;
} Vec3TrampolineValue;

// Main value structure
struct Vec3Value {
    Vec3Object object;
    union {
        Number number;
        String string;
        vec3_list* list;
        bool boolean;
        Vec3Function* function;
        HashMap* record;
        Vec3TrampolineValue* trampoline;
    } as;
};

// HashMap functions
HashMap* hashmap_new(void);
void hashmap_destroy(HashMap* map);
Vec3Value* hashmap_get(HashMap* map, const char* key);
Vec3Value* hashmap_put(HashMap* map, const char* key, Vec3Value* value);
void hashmap_remove(HashMap* map, const char* key);

// Reference counting
void vec3_incref(Vec3Value* value);
void vec3_decref(Vec3Value* value);

// Destructors
void vec3_destroy_string(Vec3Object* object);
void vec3_destroy_function(Vec3Object* object);
void vec3_destroy_record(Vec3Object* object);
void vec3_destroy_trampoline(Vec3Object* object);

// Thunk and trampoline functions
Vec3Thunk* vec3_new_thunk(Vec3Function* func, Vec3Value** args, int arg_count);
void vec3_destroy_thunk(Vec3Thunk* thunk);
Vec3TrampolineValue* vec3_new_trampoline_thunk(Vec3Thunk* thunk);
Vec3TrampolineValue* vec3_new_trampoline_value(Vec3Value* value);
Vec3Value* vec3_trampoline_eval(Vec3TrampolineValue* tramp);

// Constructors
Vec3Value* vec3_new_number(Number number);
Vec3Value* vec3_new_string(const char* chars);
Vec3Value* vec3_new_list(size_t count, ...);
Vec3Value* vec3_new_bool(bool b);
Vec3Value* vec3_new_nil(void);
Vec3Value* vec3_new_function(const char* name, int arity,
    Vec3Value* (*fn)(Vec3Value** args),
    struct Vec3Env* env);

// Function handling
void vec3_capture_variable(Vec3Function* fn, const char* name, Vec3Value* value);
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

// Record operations
Vec3Value* vec3_new_record(void);
Vec3Value* vec3_record_get(Vec3Value* record, const char* field);
Vec3Value* vec3_record_extend(Vec3Value* record, const char* field, Vec3Value* value);
void vec3_record_set(Vec3Value* record, const char* field, Vec3Value* value);
Vec3Value* vec3_record_restrict(Vec3Value* record, const char* field);

// Utility functions
void vec3_print_internal(const Vec3Value* value, bool nl);
Vec3Value* vec3_print(Vec3Value** args);
bool vec3_is_truthy(const Vec3Value* value);
Vec3Value* vec3_to_string(Vec3Value** args);
Vec3Value* vec3_to_number(Vec3Value** args);
Vec3Value* vec3_to_bool(Vec3Value** args);
Vec3Value* vec3_input(Vec3Value** args);
Vec3Value* vec3_error(Vec3Value* message);

// String operations
Vec3Value* vec3_concat(Vec3Value** args);
