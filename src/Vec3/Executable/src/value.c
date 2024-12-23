
#include "env.h"
#include "vec3_list.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_CAPACITY 16
#define TABLE_MAX_LOAD 0.75

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
void vec3_incref(Vec3Value* value)
{
    if (value != NULL) {
        value->object.ref_count++;
    }
}

void vec3_decref(Vec3Value* value)
{
    if (value != NULL) {
        assert(value->object.ref_count > 0);
        value->object.ref_count--;
        if (value->object.ref_count == 0) {
            if (value->object.destructor) {
                value->object.destructor((Vec3Object*)value);
            }
            free(value);
        }
    }
}

void vec3_destroy_string(Vec3Object* object)
{
    Vec3Value* value = (Vec3Value*)object;
    free(value->as.string.chars);
}

Vec3Value* vec3_new_number(Number number)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_NUMBER;
    value->object.ref_count = 1;
    value->object.destructor = NULL;
    value->as.number = number;
    return value;
}

Vec3Value* vec3_new_list(size_t count, ...)
{
    Vec3Value* output = malloc(sizeof(Vec3Value));
    output->object.type = TYPE_LIST;
    output->object.ref_count = 1;
    output->object.destructor = vec3_destroy_list;
    output->as.list = create_new_list(NULL);

    if (count > 0) {
        va_list ap;
        va_start(ap, count);
        Vec3Value* first = va_arg(ap, Vec3Value*);
        if (first != NULL) {
            output->as.list->value = first;
            vec3_incref(first);
            output->as.list->length = 1;
        }
        for (size_t i = 1; i < count; i++) {
            Vec3Value* arg = va_arg(ap, Vec3Value*);
            if (arg != NULL) {
                vec3_list_append(output, arg);
            }
        }
        va_end(ap);
    }

    return output;
}

Vec3Value* vec3_new_string(const char* chars)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_STRING;
    value->object.ref_count = 1;
    value->object.destructor = vec3_destroy_string;
    value->as.string.length = strlen(chars);
    value->as.string.chars = strdup(chars);
    return value;
}

Vec3Value* vec3_new_bool(bool b)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_BOOL;
    value->object.ref_count = 1;
    value->object.destructor = NULL;
    value->as.boolean = b;
    return value;
}

Vec3Value* vec3_new_nil(void)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_NIL;
    value->object.ref_count = 1;
    value->object.destructor = NULL;
    return value;
}

bool vec3_is_truthy(const Vec3Value* value)
{
    if (value == NULL || value->object.type == TYPE_NIL) {
        return false;
    }

    switch (value->object.type) {
    case TYPE_NUMBER:
        switch (value->as.number.type) {
        case NUMBER_INTEGER:
            return value->as.number.as.integer != 0;
        case NUMBER_FLOAT:
            return value->as.number.as.float_val != 0.0;
        case NUMBER_RATIONAL:
            return value->as.number.as.rational.num != 0;
        case NUMBER_COMPLEX:
            return value->as.number.as.complex.real != 0.0 || value->as.number.as.complex.imag != 0.0;
        default:
            return false;
        }
    case TYPE_STRING:
        return value->as.string.length > 0;
    case TYPE_LIST:
        return value->as.list->length > 0;
    case TYPE_BOOL:
        return value->as.boolean;
    default:
        return true;
    }
}

Vec3Value* vec3_print(Vec3Value** args)
{
    Vec3Value* value = args[0];
    vec3_print_internal(value, true);
    return vec3_new_nil();
}

void vec3_print_internal(const Vec3Value* value, bool nl)
{
    if (value == NULL) {
        printf("null");
        return;
    }

    switch (value->object.type) {
    case TYPE_NUMBER:
        number_print(&value->as.number);
        break;
    case TYPE_STRING:
        printf("\"%s\"", value->as.string.chars);
        break;
    case TYPE_LIST: {
        printf("[");
        vec3_list* current = value->as.list;
        bool first = true;
        while (current != NULL) {
            if (!first) {
                printf(", ");
            }
            if (current->value == NULL) {
                printf("null");
            } else {
                vec3_print_internal(current->value, false);
            }
            first = false;
            current = current->next;
        }
        printf("]");
        break;
    }
    case TYPE_NIL:
        printf("nil");
        break;
    case TYPE_BOOL:
        printf(value->as.boolean ? "true" : "false");
        break;
    case TYPE_FUNCTION:
        printf("<function %s>", value->as.function->name);
        break;
    case TYPE_RECORD: {
        printf("{");
        Entry** entries = value->as.record->entries;
        bool first = true;
        for (size_t i = 0; i < value->as.record->capacity; i++) {
            Entry* entry = entries[i];
            while (entry != NULL) {
                if (!first)
                    printf(", ");
                printf("%s: ", entry->key);
                vec3_print_internal(entry->value, false);
                first = false;
                entry = entry->next;
            }
        }
        printf("}");
        break;
    }
    default:
        printf("<unknown>");
        break;
    }
    if (nl) {
        printf("\n");
    }
}

Vec3Value* vec3_new_function(const char* name, int arity,
    Vec3Value* (*fn)(Vec3Value** args),
    struct Vec3Env* env)
{
    Vec3Function* function = malloc(sizeof(Vec3Function));
    function->name = strdup(name);
    function->arity = arity;
    function->fn = fn;
    function->env = env;
    function->closure.captured = NULL;
    function->closure.captured_count = 0;

    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_FUNCTION;
    value->object.ref_count = 1;
    value->object.destructor = vec3_destroy_function;
    value->as.function = function;

    return value;
}
Vec3Value* vec3_call_function(Vec3Value* function, Vec3Value** args, int argCount)
{
    if (function->object.type != TYPE_FUNCTION) {
        fprintf(stderr, "Can only call functions\n");
        return vec3_new_nil();
    }

    Vec3Function* fn = function->as.function;
    if (fn->arity != argCount) {
        fprintf(stderr, "Expected %d arguments but got %d\n", fn->arity, argCount);
        return vec3_new_nil();
    }

    return fn->fn(args);
}

void vec3_destroy_function(Vec3Object* object)
{
    Vec3Value* value = (Vec3Value*)object;
    Vec3Function* function = value->as.function;
    for (int i = 0; i < function->closure.captured_count; i++) {
        vec3_decref(function->closure.captured[i]);
    }
    free(function->closure.captured);
    free(function->name);
    if (function->env != NULL) {
        vec3_destroy_environment(function->env);
    }
    free(function);
}

void vec3_capture_variable(Vec3Function* fn, const char* name, Vec3Value* value)
{
    fn->closure.captured = realloc(fn->closure.captured,
        (fn->closure.captured_count + 1) * sizeof(Vec3Value*));

    vec3_incref(value);
    fn->closure.captured[fn->closure.captured_count++] = value;
}

Vec3Value* vec3_equal(Vec3Value** args)
{
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];
    vec3_incref(a);
    vec3_incref(b);

    bool result = false;

    if (a->object.type != b->object.type) {
        result = false;
    } else {
        switch (a->object.type) {
        case TYPE_NUMBER:
            if (a->as.number.type == b->as.number.type) {
                switch (a->as.number.type) {
                case NUMBER_INTEGER:
                    result = a->as.number.as.integer == b->as.number.as.integer;
                    break;
                case NUMBER_FLOAT:
                    result = a->as.number.as.float_val == b->as.number.as.float_val;
                    break;
                case NUMBER_RATIONAL:
                    result = (a->as.number.as.rational.num * b->as.number.as.rational.denom == b->as.number.as.rational.num * a->as.number.as.rational.denom);
                    break;
                case NUMBER_COMPLEX:
                    result = (a->as.number.as.complex.real == b->as.number.as.complex.real && a->as.number.as.complex.imag == b->as.number.as.complex.imag);
                    break;
                }
            }
            break;
        case TYPE_STRING:
            result = strcmp(a->as.string.chars, b->as.string.chars) == 0;
            break;
        case TYPE_NIL:
            result = true;
            break;
        case TYPE_BOOL:
            result = a->as.boolean == b->as.boolean;
            break;
        default:
            fprintf(stderr, "Cannot compare values of this type\n");
            break;
        }
    }

    vec3_decref(a);
    vec3_decref(b);
    return vec3_new_bool(result);
}

Vec3Value* vec3_not_equal(Vec3Value** args)
{
    Vec3Value* eq = vec3_equal(args);
    Vec3Value* result = vec3_not(&eq);
    vec3_decref(eq);
    return result;
}

Vec3Value* vec3_less(Vec3Value** args)
{
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];
    vec3_incref(a);
    vec3_incref(b);

    bool result = false;

    if (a->object.type == TYPE_NUMBER && b->object.type == TYPE_NUMBER) {
        double a_val = 0.0, b_val = 0.0;

        switch (a->as.number.type) {
        case NUMBER_INTEGER:
            a_val = (double)a->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            a_val = a->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            a_val = (double)a->as.number.as.rational.num / (double)a->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            a_val = a->as.number.as.complex.real;
            break;
        }

        switch (b->as.number.type) {
        case NUMBER_INTEGER:
            b_val = (double)b->as.number.as.integer;
            break;
        case NUMBER_FLOAT:
            b_val = b->as.number.as.float_val;
            break;
        case NUMBER_RATIONAL:
            b_val = (double)b->as.number.as.rational.num / (double)b->as.number.as.rational.denom;
            break;
        case NUMBER_COMPLEX:
            b_val = b->as.number.as.complex.real;
            break;
        }

        result = a_val < b_val;
    } else if (a->object.type == TYPE_STRING && b->object.type == TYPE_STRING) {
        result = strcmp(a->as.string.chars, b->as.string.chars) < 0;
    }

    vec3_decref(a);
    vec3_decref(b);
    return vec3_new_bool(result);
}

Vec3Value* vec3_less_equal(Vec3Value** args)
{
    Vec3Value* less = vec3_less(args);
    Vec3Value* equal = vec3_equal(args);

    bool result = vec3_is_truthy(less) || vec3_is_truthy(equal);

    vec3_decref(less);
    vec3_decref(equal);

    return vec3_new_bool(result);
}

Vec3Value* vec3_greater(Vec3Value** args)
{
    Vec3Value* b = args[0];
    Vec3Value* a = args[1];
    return vec3_less(&b);
}

Vec3Value* vec3_greater_equal(Vec3Value** args)
{
    Vec3Value* less = vec3_less(args);
    bool result = !vec3_is_truthy(less);
    vec3_decref(less);
    return vec3_new_bool(result);
}

Vec3Value* vec3_and(Vec3Value** args)
{
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];
    vec3_incref(a);
    vec3_incref(b);

    bool result = vec3_is_truthy(a) && vec3_is_truthy(b);

    vec3_decref(a);
    vec3_decref(b);

    return vec3_new_bool(result);
}

Vec3Value* vec3_or(Vec3Value** args)
{
    Vec3Value* a = args[0];
    Vec3Value* b = args[1];
    vec3_incref(a);
    vec3_incref(b);

    bool result = vec3_is_truthy(a) || vec3_is_truthy(b);

    vec3_decref(a);
    vec3_decref(b);

    return vec3_new_bool(result);
}

Vec3Value* vec3_not(Vec3Value** args)
{
    Vec3Value* value = args[0];
    vec3_incref(value);
    bool result = !vec3_is_truthy(value);
    vec3_decref(value);
    return vec3_new_bool(result);
}

Vec3Value* vec3_input(Vec3Value** args)
{
    char buffer[1024];
    if (fgets(buffer, sizeof(buffer), stdin)) {
        size_t len = strlen(buffer);
        if (len > 0 && buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
        }
        return vec3_new_string(buffer);
    }
    return vec3_new_nil();
}

Vec3Value* vec3_error(Vec3Value* message)
{
    if (message->object.type == TYPE_STRING) {
        fprintf(stderr, "Error: %s\n", message->as.string.chars);
    } else {
        fprintf(stderr, "Error\n");
    }
    return vec3_new_nil();
}

Vec3Value* vec3_to_string(Vec3Value** args)
{
    return vec3_new_nil();
}

Vec3Value* vec3_to_number(Vec3Value** args)
{
    return vec3_new_nil();
}

Vec3Value* vec3_to_bool(Vec3Value** args)
{
    Vec3Value* value = args[0];
    vec3_incref(value);
    bool result = vec3_is_truthy(value);
    vec3_decref(value);
    return vec3_new_bool(result);
}

// record
Vec3Value* vec3_new_record(void)
{
    Vec3Value* value = malloc(sizeof(Vec3Value));
    value->object.type = TYPE_RECORD;
    value->object.ref_count = 1;
    value->object.destructor = vec3_destroy_record;
    value->as.record = hashmap_new();
    return value;
}

void vec3_destroy_record(Vec3Object* object)
{
    Vec3Value* value = (Vec3Value*)object;
    hashmap_destroy(value->as.record);
}

Vec3Value* vec3_record_get(Vec3Value* record, const char* field)
{
    if (record->object.type != TYPE_RECORD) {
        fprintf(stderr, "Expected record\n");
        return vec3_new_nil();
    }
    Vec3Value* value = hashmap_get(record->as.record, field);
    return value != NULL ? value : vec3_new_nil();
}

void vec3_record_set(Vec3Value* record, const char* field, Vec3Value* value)
{
    if (record->object.type != TYPE_RECORD) {
        fprintf(stderr, "Expected record\n");
        return;
    }
    vec3_incref(value);
    Vec3Value* old = hashmap_put(record->as.record, field, value);
    if (old != NULL) {
        vec3_decref(old);
    }
}

Vec3Value* vec3_record_extend(Vec3Value* record, const char* field, Vec3Value* value)
{
    Vec3Value* new_record = vec3_new_record();

    if (record->object.type == TYPE_RECORD) {
        Entry** entries = record->as.record->entries;
        for (size_t i = 0; i < record->as.record->capacity; i++) {
            Entry* entry = entries[i];
            while (entry != NULL) {
                vec3_record_set(new_record, entry->key, entry->value);
                entry = entry->next;
            }
        }
    }

    vec3_record_set(new_record, field, value);
    return new_record;
}

Vec3Value* vec3_record_restrict(Vec3Value* record, const char* field)
{
    Vec3Value* new_record = vec3_new_record();

    if (record->object.type == TYPE_RECORD) {
        Entry** entries = record->as.record->entries;
        for (size_t i = 0; i < record->as.record->capacity; i++) {
            Entry* entry = entries[i];
            while (entry != NULL) {
                if (strcmp(entry->key, field) != 0) {
                    vec3_record_set(new_record, entry->key, entry->value);
                }
                entry = entry->next;
            }
        }
    }

    return new_record;
}
