
#include "number.h"
#include "value.h"
#include "vec3_math.h"
#include "vec3_list.h"
#include "env.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

Vec3Env* env;

// Forward declarations
Vec3Value* rec_func(Vec3Value** args);

// Function implementations 

                    Vec3Value* rec_func(Vec3Value** args) {
                        Vec3Function* func = ((Vec3Value*)args[0])->as.function;
                        Vec3Env* func_env = vec3_new_environment(func->env);
                        
                        // Bind parameters
                        vec3_env_define(func_env, "n", args[1]);
                        
                        // Execute body
                        Vec3Value* result = ({
               Vec3Value* cond_val = vec3_call_function(vec3_env_get(env, "lt"), (Vec3Value*[]){vec3_env_get(env, "lt"), ({
                           Vec3Value* value = ({
                   Vec3Value* value = vec3_env_get(env, "n");
                   if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                       value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                   }
                   value;
               });
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       }), ({
                           Vec3Value* value = vec3_new_number(number_from_int(1));
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 3);
               if (cond_val != NULL && cond_val->object.type == TYPE_TRAMPOLINE && cond_val->as.trampoline->is_thunk) {
                   cond_val = vec3_trampoline_eval((Vec3TrampolineValue*)cond_val);
               }
               vec3_is_truthy(cond_val) ? vec3_new_number(number_from_int(1)) : vec3_call_function(vec3_env_get(env, "add"), (Vec3Value*[]){vec3_env_get(env, "add"), ({
                           Vec3Value* value = ({
                   Vec3Value* value = vec3_env_get(env, "n");
                   if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                       value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                   }
                   value;
               });
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       }), ({
                           Vec3Value* value = (Vec3Value*)vec3_new_trampoline_thunk(
                   vec3_new_thunk(((Vec3Value*)vec3_env_get(env, "fact"))->as.function, 
                       (Vec3Value*[]){vec3_env_get(env, "fact"), ({
                           Vec3Value* value = vec3_call_function(vec3_env_get(env, "sub"), (Vec3Value*[]){vec3_env_get(env, "sub"), ({
                           Vec3Value* value = ({
                   Vec3Value* value = vec3_env_get(env, "n");
                   if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                       value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                   }
                   value;
               });
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       }), ({
                           Vec3Value* value = vec3_new_number(number_from_int(1));
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 3);
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 
                       2));
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 3);
           });
                        
                        // If we got a thunk back, evaluate it to get the final value
                        if (result != NULL && result->object.type == TYPE_TRAMPOLINE && result->as.trampoline->is_thunk) {
                            result = vec3_trampoline_eval((Vec3TrampolineValue*)result);
                        }
                        
                        vec3_destroy_environment(func_env);
                        return result;
                    }

int main(void) {
    // Create global environment
    env = vec3_new_environment(NULL);
    
    // Register builtin functions
    vec3_register_builtins(env);
    
    // Program statements
    
    {
                    Vec3Value* func = vec3_new_function("rec_func", 1, rec_func, env);
                    func->as.function->is_recursive = true;
                    vec3_env_define(env, "fact", func);
                }
    {
                    Vec3Value* value = (Vec3Value*)vec3_new_trampoline_thunk(
                   vec3_new_thunk(((Vec3Value*)vec3_env_get(env, "fact"))->as.function, 
                       (Vec3Value*[]){vec3_env_get(env, "fact"), ({
                           Vec3Value* value = vec3_new_number(number_from_int(5));
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 
                       2));
                    vec3_env_define(env, "x", value);
                }
    { Vec3Value* temp = vec3_call_function(vec3_env_get(env, "print"), (Vec3Value*[]){vec3_env_get(env, "print"), ({
                           Vec3Value* value = ({
                   Vec3Value* value = vec3_env_get(env, "x");
                   if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                       value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                   }
                   value;
               });
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 2); }
    {
                    Vec3Value* value = (Vec3Value*)vec3_new_trampoline_thunk(
                   vec3_new_thunk(((Vec3Value*)vec3_env_get(env, "fact"))->as.function, 
                       (Vec3Value*[]){vec3_env_get(env, "fact"), ({
                           Vec3Value* value = vec3_new_number(number_from_int(1000000));
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 
                       2));
                    vec3_env_define(env, "y", value);
                }
    { Vec3Value* temp = vec3_call_function(vec3_env_get(env, "print"), (Vec3Value*[]){vec3_env_get(env, "print"), ({
                           Vec3Value* value = ({
                   Vec3Value* value = vec3_env_get(env, "y");
                   if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                       value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                   }
                   value;
               });
                           if (value != NULL && value->object.type == TYPE_TRAMPOLINE && value->as.trampoline->is_thunk) {
                               value = vec3_trampoline_eval((Vec3TrampolineValue*)value);
                           }
                           value;
                       })}, 2); }
    
    return 0;
}
