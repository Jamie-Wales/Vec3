#include "env.h"
#include "value.h"  // Get full definitions
#include "vec3_list.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Vec3Env* vec3_new_environment(Vec3Env* enclosing)
{
    Vec3Env* env = malloc(sizeof(Vec3Env));
    if (env == NULL) {
        fprintf(stderr, "Could not allocate environment\n");
        exit(1);
    }
    env->enclosing = enclosing;
    env->values = hashmap_new();
    return env;
}

void vec3_env_define(Vec3Env* env, const char* name, Vec3Value* value)
{
    if (value == NULL) {
        fprintf(stderr, "Cannot define NULL value for %s\n", name);
        return;
    }

    vec3_incref(value);
    Vec3Value* existing = hashmap_put(env->values, name, value);

    if (existing != NULL) {
        vec3_decref(existing);
    }
}

bool vec3_env_assign(Vec3Env* env, const char* name, Vec3Value* value)
{
    if (hashmap_get(env->values, name) != NULL) {
        vec3_env_define(env, name, value); // Will handle ref counting
        return true;
    }

    if (env->enclosing != NULL) {
        return vec3_env_assign(env->enclosing, name, value);
    }

    return false;
}

Vec3Value* vec3_env_get(Vec3Env* env, const char* name)
{
    Vec3Value* value = hashmap_get(env->values, name);

    if (value != NULL) {
        return value;
    }

    if (env->enclosing != NULL) {
        return vec3_env_get(env->enclosing, name);
    }

    return NULL; 
}

void vec3_destroy_environment(Vec3Env* env)
{
    hashmap_destroy(env->values); 
    free(env);
}

void vec3_register_builtins(struct Vec3Env* env)
{
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
    vec3_env_define(env, "add", vec3_new_function("add", 2, vec3_add, NULL));
    vec3_env_define(env, "mul", vec3_new_function("mul", 2, vec3_multiply, NULL));
    vec3_env_define(env, "pow", vec3_new_function("power", 2, vec3_power, NULL));
    vec3_env_define(env, "eq", vec3_new_function("equal", 2, vec3_equal, NULL));
    vec3_env_define(env, "neq", vec3_new_function("not_equal", 2, vec3_not_equal, NULL));
    vec3_env_define(env, "lt", vec3_new_function("less", 2, vec3_less, NULL));
    vec3_env_define(env, "lte", vec3_new_function("less_equal", 2, vec3_less_equal, NULL));
    vec3_env_define(env, "gt", vec3_new_function("greater", 2, vec3_greater, NULL));
    vec3_env_define(env, "gte", vec3_new_function("greater_equal", 2, vec3_greater_equal, NULL));

    vec3_env_define(env, "cons", vec3_new_function("cons", 2, vec3_cons, NULL));
    vec3_env_define(env, "index", vec3_new_function("index", 2, vec3_index, NULL));
    vec3_env_define(env, "select", vec3_new_function("select", 2, vec3_select, NULL));
    
    vec3_env_define(env, "input", vec3_new_function("input", 0, vec3_input, NULL));
    vec3_env_define(env, "print", vec3_new_function("print", 1, vec3_print, NULL));
    vec3_env_define(env, "plot", vec3_new_function("plot", 1, vec3_plot, NULL));
    vec3_env_define(env, "PI", vec3_new_number(number_from_float(VEC3_PI)));
    vec3_env_define(env, "E", vec3_new_number(number_from_float(VEC3_E)));
    vec3_env_define(env, "TAU", vec3_new_number(number_from_float(VEC3_TAU)));
}
