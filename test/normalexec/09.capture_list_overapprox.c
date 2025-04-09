#include <malloc.h>
#include <stdio.h>

#define CAST_LAMBDA(name, from_type, to_type, func_type) ((__perkelang_capture_dummy_##from_type = name, (to_type) {__perkelang_capture_dummy_##from_type.env, (func_type)__perkelang_capture_dummy_##from_type.func}))
#define CALL_LAMBDA0(l, t) (__perkelang_capture_dummy_##t = l, __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env)))
#define CALL_LAMBDA(l, t, ...) (__perkelang_capture_dummy_##t = l, __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env), __VA_ARGS__))


typedef void (*l_char_ptr__vararg_to_void_r)(char*, ...);
typedef void (*l_void_ptr_to_void_r)(void*);
typedef void* void_ptr;
typedef int (*l__to_int_r)();
typedef char* char_ptr;
typedef struct env_ {void* _0; void* _1; void* _2; void* _3; void* _4;} env_;
typedef struct l_void_ptr_to_void_r_env_ {
    struct env_ env;
    l_void_ptr_to_void_r func;
} l_void_ptr_to_void_r_env_;
struct l_void_ptr_to_void_r_env_ __perkelang_capture_dummy_l_void_ptr_to_void_r_env_;
typedef void (*l_void_ptr__l_void_ptr_to_void_r_env__to_void_r)(void*, l_void_ptr_to_void_r_env_);

typedef struct l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_ {
    struct env_ env;
    l_void_ptr__l_void_ptr_to_void_r_env__to_void_r func;
} l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_;
struct l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_ __perkelang_capture_dummy_l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_;

int main ();
static void __perkelang_lambda_1 (void*);
static void __perkelang_lambda_0 (void*, l_void_ptr_to_void_r_env_);

int main() {
    int x = 10;
    l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_ draw = {{}, (l_void_ptr__l_void_ptr_to_void_r_env__to_void_r) __perkelang_lambda_0};
    CALL_LAMBDA(draw, l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_, ((l_void_ptr_to_void_r_env_){{(void*)x}, (l_void_ptr_to_void_r) __perkelang_lambda_1}));
    return 0;
}

static void __perkelang_lambda_1(void* __env) {
    env_* _env = (env_*) __env;
    int x = (int)_env->_0;
    printf("x = %d\n", x);
}

static void __perkelang_lambda_0(void* __env, l_void_ptr_to_void_r_env_ f) {
    printf("Panano\n");
    CALL_LAMBDA0(f, l_void_ptr_to_void_r_env_);
}
