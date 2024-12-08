#include "env.h"
#include "hashmap.h"
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

    // If not found in current environment, check enclosing one
    if (env->enclosing != NULL) {
        return vec3_env_get(env->enclosing, name);
    }

    return NULL; // Variable not found
}

void vec3_destroy_environment(Vec3Env* env)
{
    hashmap_destroy(env->values); // This will handle decrementing ref counts
    free(env);
}
