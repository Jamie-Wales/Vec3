
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


int main(void) {

    // Create global environment
    Vec3Env* env = vec3_new_environment(NULL);
    
    // Register builtin functions
    vec3_register_builtins(env);

    
    // Program statements
    vec3_env_define(env, "a", vec3_new_number(number_from_int(10)));
    vec3_call_function(vec3_env_get(env, "print"), (Vec3Value*[]){vec3_new_number(number_from_int(10))}, 1);
    

    // Cleanup environment
    vec3_destroy_environment(env);
    return 0;

}
