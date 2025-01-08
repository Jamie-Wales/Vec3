#pragma once
#include "value.h"
#include <stdbool.h>
#include "vec3_math.h"
struct Vec3Env {
    struct Vec3Env* enclosing;
    HashMap* values;
};
struct Vec3Env* vec3_new_environment(struct Vec3Env* enclosing);
void vec3_env_define(struct Vec3Env* env, const char* name, Vec3Value* value);
bool vec3_env_assign(struct Vec3Env* env, const char* name, Vec3Value* value);
Vec3Value* vec3_env_get(struct Vec3Env* env, const char* name);
void vec3_destroy_environment(struct Vec3Env* env);

void vec3_register_builtins(struct Vec3Env* env);