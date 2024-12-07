
#include "value.h"

int main(void)
{
    Vec3Value* v = vec3_new_list(vec3_new_nil(), vec3_new_nil());
    vec3_print(v);
};
