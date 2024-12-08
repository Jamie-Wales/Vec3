#pragma once
#include "value.h"
#include <stddef.h>

#define TABLE_MAX_LOAD 0.75

typedef struct Entry {
    char* key;
    Vec3Value* value;
    struct Entry* next;
} Entry;

typedef struct HashMap {
    Entry** entries;
    size_t capacity;
    size_t count;
} HashMap;

HashMap* hashmap_new(void);
void hashmap_destroy(HashMap* map);
Vec3Value* hashmap_get(HashMap* map, const char* key);
Vec3Value* hashmap_put(HashMap* map, const char* key, Vec3Value* value);
void hashmap_remove(HashMap* map, const char* key);
