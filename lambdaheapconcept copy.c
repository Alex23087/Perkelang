#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct
{
    int x;
    int y;
} lambda_type;

int f(lambda_type *lambda)
{
    int x = lambda->x;
    int y = lambda->y;
    printf("x: %d, y: %d\n", x, y);
    return x + y;
}

void *alloclabmd(int size, void *labmda)
{
    void *ptr = malloc(size);
    memcpy(ptr, labmda, size);
    return ptr;
}

int main()
{
    // f(() = > {});
    int env = 42;
    int g = 69;
    f(alloclabmd(sizeof(lambda_type), (void *)&((lambda_type){env, g})));
}

// coolio
// hahmhihshh paradize

// fun (float * float).add(other: (float * float)): (float * float) {
//     return (self[0] + other[0], self[1] + other[1]
// }