#pragma once
#include "value.h"
#include "vec3_math.h"  // Add this to get math functions and constants
#include <stdbool.h>

// Environment management functions
struct Vec3Env* vec3_new_environment(struct Vec3Env* enclosing);
void vec3_env_define(struct Vec3Env* env, const char* name, Vec3Value* value);
bool vec3_env_assign(struct Vec3Env* env, const char* name, Vec3Value* value);
Vec3Value* vec3_env_get(struct Vec3Env* env, const char* name);
void vec3_destroy_environment(struct Vec3Env* env);

// Builtin registration
void vec3_register_builtins(struct Vec3Env* env)
{
    // Math functions
    vec3_env_define(env, "BUILTIN_ROOT", vec3_new_function("root", 1, vec3_sqrt, NULL));
    vec3_env_define(env, "BUILTIN_ABS", vec3_new_function("abs", 1, vec3_abs, NULL));
    vec3_env_define(env, "BUILTIN_FLOOR", vec3_new_function("floor", 1, vec3_floor, NULL));
    vec3_env_define(env, "BUILTIN_COS", vec3_new_function("cos", 1, vec3_cos, NULL));
    vec3_env_define(env, "BUILTIN_SIN", vec3_new_function("sin", 1, vec3_sin, NULL));
    vec3_env_define(env, "BUILTIN_TAN", vec3_new_function("tan", 1, vec3_tan, NULL));
    vec3_env_define(env, "BUILTIN_ACOS", vec3_new_function("acos", 1, vec3_acos, NULL));
    vec3_env_define(env, "BUILTIN_ASIN", vec3_new_function("asin", 1, vec3_asin, NULL));
    vec3_env_define(env, "BUILTIN_ATAN", vec3_new_function("atan", 1, vec3_atan, NULL));
    vec3_env_define(env, "BUILTIN_EXP", vec3_new_function("exp", 1, vec3_exp, NULL));
    vec3_env_define(env, "BUILTIN_LOG", vec3_new_function("log", 2, vec3_log, NULL));
    vec3_env_define(env, "BUILTIN_LOG10", vec3_new_function("log10", 1, vec3_log10, NULL));

    // Operators
    vec3_env_define(env, "+", vec3_new_function("add", 2, vec3_add, NULL));
    vec3_env_define(env, "*", vec3_new_function("multiply", 2, vec3_multiply, NULL));
    vec3_env_define(env, "**", vec3_new_function("power", 2, vec3_power, NULL));
    vec3_env_define(env, "==", vec3_new_function("equal", 2, vec3_equal, NULL));
    vec3_env_define(env, "!=", vec3_new_function("not_equal", 2, vec3_not_equal, NULL));
    vec3_env_define(env, "<", vec3_new_function("less", 2, vec3_less, NULL));
    vec3_env_define(env, "<=", vec3_new_function("less_equal", 2, vec3_less_equal, NULL));
    vec3_env_define(env, ">", vec3_new_function("greater", 2, vec3_greater, NULL));
    vec3_env_define(env, ">=", vec3_new_function("greater_equal", 2, vec3_greater_equal, NULL));

    // Constants
    vec3_env_define(env, "PI", vec3_new_number(number_from_float(VEC3_PI)));
    vec3_env_define(env, "E", vec3_new_number(number_from_float(VEC3_E)));
    vec3_env_define(env, "TAU", vec3_new_number(number_from_float(VEC3_TAU)));
}