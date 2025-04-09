#include <malloc.h>
#include <stdio.h>

typedef void (*l_char_ptr__vararg_to_void_r)(char*, ...);
typedef void (*l_void_ptr_to_void_r)(void*);
typedef void* void_ptr;
typedef int (*l__to_int_r)();
typedef char* char_ptr;
typedef void (*l_void_ptr__Test_to_void_r)(void*, void*);
typedef struct capturing_ {;} capturing_;
typedef struct l_void_ptr_to_void_r_capturing_ {struct capturing_ env; l_void_ptr_to_void_r func;} l_void_ptr_to_void_r_capturing_;;
struct l_void_ptr_to_void_r_capturing_ __perkelang_capture_dummy_l_void_ptr_to_void_r_capturing_;
;
typedef struct l_void_ptr__Test_to_void_r_capturing_ {struct capturing_ env; l_void_ptr__Test_to_void_r func;} l_void_ptr__Test_to_void_r_capturing_;;
struct l_void_ptr__Test_to_void_r_capturing_ __perkelang_capture_dummy_l_void_ptr__Test_to_void_r_capturing_;

struct Test {
    l_void_ptr__Test_to_void_r_capturing_ constructor;
};
typedef struct Test* Test;


int main ();
static void __perkelang_lambda_0 (void*, Test);

Test Test_init() {
    Test self = malloc(sizeof(struct Test));
    self->constructor = (l_void_ptr__Test_to_void_r_capturing_) {{}, (l_void_ptr__Test_to_void_r) __perkelang_lambda_0};

    (__perkelang_capture_dummy_l_void_ptr__Test_to_void_r_capturing_ = self->constructor, __perkelang_capture_dummy_l_void_ptr__Test_to_void_r_capturing_.func(self, &(__perkelang_capture_dummy_l_void_ptr__Test_to_void_r_capturing_.env)));
    return self;
}

int main() {
    Test x = Test_init();
    return 0;
}

static void __perkelang_lambda_0(void* __env, Test self) {
    printf("Test constructor\n");
}
