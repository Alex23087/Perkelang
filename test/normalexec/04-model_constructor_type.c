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
typedef void (*l_void_ptr__Test_to_void_r)(void*, void*);
typedef struct env_ {;} env_;
typedef struct l_void_ptr_to_void_r_env_ {
    struct env_ env;
    l_void_ptr_to_void_r func;
} l_void_ptr_to_void_r_env_;
struct l_void_ptr_to_void_r_env_ __perkelang_capture_dummy_l_void_ptr_to_void_r_env_;

typedef struct l_void_ptr__Test_to_void_r_env_ {
    struct env_ env;
    l_void_ptr__Test_to_void_r func;
} l_void_ptr__Test_to_void_r_env_;
struct l_void_ptr__Test_to_void_r_env_ __perkelang_capture_dummy_l_void_ptr__Test_to_void_r_env_;

struct Test {
    l_void_ptr__Test_to_void_r_env_ constructor;
};
typedef struct Test* Test;


int main ();
static void __perkelang_lambda_0 (void*, Test);

Test Test_init() {
    Test self = malloc(sizeof(struct Test));
    self->constructor = (l_void_ptr__Test_to_void_r_env_) {{}, (l_void_ptr__Test_to_void_r) __perkelang_lambda_0};

    (__perkelang_capture_dummy_l_void_ptr__Test_to_void_r_env_ = self->constructor, __perkelang_capture_dummy_l_void_ptr__Test_to_void_r_env_.func(self, &(__perkelang_capture_dummy_l_void_ptr__Test_to_void_r_env_.env)));
    return self;
}

int main() {
    Test x = Test_init();
    return 0;
}

static void __perkelang_lambda_0(void* __env, Test self) {
    printf("Test constructor\n");
}
