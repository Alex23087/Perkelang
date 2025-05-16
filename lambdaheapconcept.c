#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct _lambdummy_type
{
    void *env;
    void *func;
} __lambdummy_type;
static __lambdummy_type *__lambdummy;

typedef int (*l_int_to_int_r)(void *env, int z);

#define CALL_LAMBDA0(l, t) (__lambdummy = (__lambdummy_type *)l, ((t)(__lambdummy->func))(__lambdummy->env))
#define CALL_LAMBDA(l, t, ...) (__lambdummy = (__lambdummy_type *)l,       \
                                ((t)(__lambdummy->func))(__lambdummy->env, \
                                                         __VA_ARGS__))

typedef struct
{
    int x; // captured vars
    int y;
} env_type;

struct lambda_type
{
    void *env;
    l_int_to_int_r func; // lambda takes env and params
};
typedef struct lambda_type *lambda_type;

void f(lambda_type lambda)
{
    printf("result: %d\n", CALL_LAMBDA(lambda, l_int_to_int_r, 0));
}

__lambdummy_type *alloclabmd(int size, void *labmda, void *env)
{
    __lambdummy_type *ptr = malloc(sizeof(__lambdummy_type));
    ptr->env = malloc(size);
    memcpy(ptr->env, env, size);
    ptr->func = labmda;
    return ptr;
}

static int sum(void *env, int z);

int main()
{
    // f(() = > {});
    int fvar = 42;
    int fvar_2 = 69;
    f((lambda_type)alloclabmd(sizeof(env_type), (void *)&sum, (void *)&((env_type){fvar, fvar_2})));
}

// coolio
// hahmhihshh paradize

// fun (float * float).add(other: (float * float)): (float * float) {
//     return (self[0] + other[0], self[1] + other[1]
// }

static int sum(void *env, int z)
{
    env_type *_env = (env_type *)env;
    int x = _env->x;
    int y = _env->y;
    return x + y + z;
}