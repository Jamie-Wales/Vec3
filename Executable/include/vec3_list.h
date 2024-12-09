#pragma once
#include "value.h"
#include <stdlib.h>
#include <string.h>

vec3_list* create_new_list(Vec3Value* value);
void vec3_destroy_list(Vec3Object* object);
void vec3_list_append(Vec3Value* list_value, Vec3Value* value);
Vec3Value* vec3_list_get(Vec3Value* list_value, size_t index);
void vec3_list_set(Vec3Value* list_value, size_t index, Vec3Value* value);

Vec3Value* vec3_cons(Vec3Value** args);    
Vec3Value* vec3_index(Vec3Value** args);   
Vec3Value* vec3_select(Vec3Value** args);   