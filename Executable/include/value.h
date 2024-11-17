
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

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

typedef struct Vec3Value {
    Vec3Object object;
    union {
        struct {
            enum {
                NUMBER_INTEGER,
                NUMBER_FLOAT,
                NUMBER_RATIONAL,
                NUMBER_COMPLEX
            } type;
            union {
                int64_t integer;
                double float_val;
                struct {
                    int64_t num;
                    int64_t denom;
                } rational;
                struct {
                    double real;
                    double imag;
                } complex;
            } as;
        } number;

        struct {
            char* chars;
            size_t length;
        } string;
    };
} Vec3Value;

void vec3_incref(Vec3Value* value);
void vec3_decref(Vec3Value* value);
Vec3Value* vec3_create_integer(int64_t value);
Vec3Value* vec3_add(Vec3Value* a, Vec3Value* b);
