#include "hashmap.h"
#include <stdlib.h>
#include <string.h>

#define INITIAL_CAPACITY 16

static uint32_t hash_string(const char* key)
{
    uint32_t hash = 2166136261u;
    for (const char* p = key; *p != '\0'; p++) {
        hash ^= (uint32_t)*p;
        hash *= 16777619;
    }
    return hash;
}

static void hashmap_resize(HashMap* map, size_t new_capacity)
{
    Entry** new_entries = calloc(new_capacity, sizeof(Entry*));

    for (size_t i = 0; i < map->capacity; i++) {
        Entry* entry = map->entries[i];
        while (entry != NULL) {
            Entry* next = entry->next;
            uint32_t hash = hash_string(entry->key) % new_capacity;
            entry->next = new_entries[hash];
            new_entries[hash] = entry;
            entry = next;
        }
    }

    free(map->entries);
    map->entries = new_entries;
    map->capacity = new_capacity;
}

HashMap* hashmap_new(void)
{
    HashMap* map = malloc(sizeof(HashMap));
    map->capacity = INITIAL_CAPACITY;
    map->count = 0;
    map->entries = calloc(INITIAL_CAPACITY, sizeof(Entry*));
    return map;
}

void hashmap_destroy(HashMap* map)
{
    for (size_t i = 0; i < map->capacity; i++) {
        Entry* entry = map->entries[i];
        while (entry != NULL) {
            Entry* next = entry->next;
            vec3_decref(entry->value);
            free(entry->key);
            free(entry);
            entry = next;
        }
    }
    free(map->entries);
    free(map);
}

Vec3Value* hashmap_get(HashMap* map, const char* key)
{
    uint32_t hash = hash_string(key) % map->capacity;
    Entry* entry = map->entries[hash];

    while (entry != NULL) {
        if (strcmp(entry->key, key) == 0) {
            return entry->value;
        }
        entry = entry->next;
    }

    return NULL;
}

Vec3Value* hashmap_put(HashMap* map, const char* key, Vec3Value* value)
{
    if (map->count >= map->capacity * TABLE_MAX_LOAD) {
        hashmap_resize(map, map->capacity * 2);
    }

    uint32_t hash = hash_string(key) % map->capacity;
    Entry* entry = map->entries[hash];

    while (entry != NULL) {
        if (strcmp(entry->key, key) == 0) {
            Vec3Value* old_value = entry->value;
            entry->value = value;
            return old_value;
        }
        entry = entry->next;
    }

    entry = malloc(sizeof(Entry));
    entry->key = strdup(key);
    entry->value = value;
    entry->next = map->entries[hash];
    map->entries[hash] = entry;
    map->count++;

    return NULL;
}

void hashmap_remove(HashMap* map, const char* key)
{
    uint32_t hash = hash_string(key) % map->capacity;
    Entry** entry_ptr = &map->entries[hash];

    while (*entry_ptr != NULL) {
        Entry* entry = *entry_ptr;
        if (strcmp(entry->key, key) == 0) {
            *entry_ptr = entry->next;
            vec3_decref(entry->value);
            free(entry->key);
            free(entry);
            map->count--;
            return;
        }
        entry_ptr = &entry->next;
    }
}
