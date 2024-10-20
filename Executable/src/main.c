#include "value.h"
#include <stdio.h>
int main(void) {
    Value a = add(10, 20);
    printf("%f", a.as.number);
}