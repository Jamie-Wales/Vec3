
#include "vec3_list.h"
#include "value.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

vec3_list* create_new_list(Vec3Value* value)
{
    vec3_list* list = malloc(sizeof(vec3_list));
    if (list == NULL) {
        fprintf(stderr, "Cannot allocate memory for list\n");
        exit(1);
    }

    list->value = value;
    if (value != NULL) {
        vec3_incref(value);
    }

    list->prev = NULL;
    list->next = NULL;
    list->length = value != NULL ? 1 : 0;

    return list;
}

void vec3_destroy_list(Vec3Object* object)
{
    Vec3Value* value = (Vec3Value*)object;
    vec3_list* current = value->as.list;
    while (current != NULL) {
        vec3_list* next = current->next;
        vec3_decref(current->value);
        free(current);
        current = next;
    }

    free(value);
}

void vec3_list_append(Vec3Value* list_value, Vec3Value* value)
{
    assert(list_value->object.type == TYPE_LIST);
    vec3_list* list = list_value->as.list;
    vec3_list* current = list;
    while (current->next != NULL) {
        current = current->next;
    }
    vec3_list* new_node = malloc(sizeof(vec3_list));
    if (new_node == NULL) {
        fprintf(stderr, "Cannot allocate memory for list node\n");
        exit(1);
    }
    new_node->value = value;
    vec3_incref(value);
    new_node->next = NULL;
    new_node->prev = current;
    new_node->length = 1;

    current->next = new_node;

    current = list;
    while (current != NULL) {
        current->length++;
        current = current->next;
    }
}

Vec3Value* vec3_list_get(Vec3Value* list_value, size_t index)
{
    assert(list_value->object.type == TYPE_LIST);
    vec3_list* list = list_value->as.list;

    if (index >= list->length) {
        return vec3_new_nil();
    }

    vec3_list* current = list;
    for (size_t i = 0; i < index; i++) {
        current = current->next;
        if (current == NULL) {
            return vec3_new_nil();
        }
    }

    vec3_incref(current->value);
    return current->value;
}

void vec3_list_set(Vec3Value* list_value, size_t index, Vec3Value* value)
{
    assert(list_value->object.type == TYPE_LIST);
    vec3_list* list = list_value->as.list;

    if (index >= list->length) {
        return;
    }
    vec3_list* current = list;
    for (size_t i = 0; i < index; i++) {
        current = current->next;
        if (current == NULL) {
            return;
        }
    }

    vec3_incref(value);
    Vec3Value* va = current->value;
    current->value = value;
    vec3_decref(va);
}

Vec3Value* vec3_cons(Vec3Value* value, Vec3Value* list_value)
{
    if (list_value->object.type != TYPE_LIST) {
        return vec3_new_nil();
    }
    Vec3Value* new_list_value = malloc(sizeof(Vec3Value));
    new_list_value->object.type = TYPE_LIST;
    new_list_value->object.ref_count = 1;
    new_list_value->object.destructor = vec3_destroy_list;
    vec3_list* new_head = malloc(sizeof(vec3_list));
    if (new_head == NULL) {
        fprintf(stderr, "Cannot allocate memory for list node\n");
        exit(1);
    }
    new_head->value = value;
    vec3_incref(value);
    new_head->prev = NULL;
    new_head->length = list_value->as.list->length + 1;
    new_head->next = list_value->as.list;
    if (list_value->as.list != NULL) {
        list_value->as.list->prev = new_head;
    }
    new_list_value->as.list = new_head;
    return new_list_value;
}

// Get element at numeric index
Vec3Value* vec3_index(Vec3Value* list_value, Vec3Value* index_value)
{
    if (list_value->object.type != TYPE_LIST || index_value->object.type != TYPE_NUMBER || index_value->as.number.type != NUMBER_INTEGER) {
        return vec3_new_nil();
    }

    long long index = index_value->as.number.as.integer;
    if (index < 0) {
        index = (int64_t)list_value->as.list->length + index;
    }

    if (index < 0 || (size_t)index >= list_value->as.list->length) {
        return vec3_new_nil();
    }

    vec3_list* current = list_value->as.list;
    for (long long i = 0; i < index; i++) {
        current = current->next;
        if (current == NULL) {
            return vec3_new_nil();
        }
    }

    vec3_incref(current->value);
    return current->value;
}

Vec3Value* vec3_select(Vec3Value* list_value, Vec3Value* key_value)
{
    if (list_value->object.type != TYPE_LIST) {
        return vec3_new_nil();
    }

    Vec3Value* result = vec3_new_list(1, vec3_new_nil());
    vec3_list* current = list_value->as.list;

    if (key_value->object.type == TYPE_NUMBER) {
        return vec3_index(list_value, key_value);
    } else if (key_value->object.type == TYPE_STRING) {
        while (current != NULL) {
            if (current->value->object.type == TYPE_STRING && strcmp(current->value->as.string.chars, key_value->as.string.chars) == 0) {
                vec3_list_append(result, current->value);
            }
            current = current->next;
        }
    }
    // If key is a boolean, select elements matching that boolean
    else if (key_value->object.type == TYPE_BOOL) {
        while (current != NULL) {
            if (current->value->object.type == TYPE_BOOL && current->value->as.boolean == key_value->as.boolean) {
                vec3_list_append(result, current->value);
            }
            current = current->next;
        }
    } else {
        vec3_decref(result);
        return vec3_new_nil();
    }

    return result;
}
