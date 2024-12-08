#pragma once
#include "value.h"

// Math functions
Vec3Value* vec3_add(Vec3Value** args);
Vec3Value* vec3_multiply(Vec3Value** args);
Vec3Value* vec3_power(Vec3Value** args);
Vec3Value* vec3_sqrt(Vec3Value** args);
Vec3Value* vec3_abs(Vec3Value** args);
Vec3Value* vec3_floor(Vec3Value** args);

// Trigonometric functions
Vec3Value* vec3_cos(Vec3Value** args);
Vec3Value* vec3_sin(Vec3Value** args);
Vec3Value* vec3_tan(Vec3Value** args);
Vec3Value* vec3_acos(Vec3Value** args);
Vec3Value* vec3_asin(Vec3Value** args);
Vec3Value* vec3_atan(Vec3Value** args);

// Exponential and logarithmic
Vec3Value* vec3_exp(Vec3Value** args);
Vec3Value* vec3_log(Vec3Value** args);
Vec3Value* vec3_log10(Vec3Value** args);

// Vector operations
Vec3Value* vec3_dot_product(Vec3Value** args);
Vec3Value* vec3_cross_product(Vec3Value** args);

// Constants
#define VEC3_PI 3.14159265358979323846
#define VEC3_E 2.71828182845904523536
#define VEC3_TAU 6.28318530717958647692