#include <malloc.h>
#include <malloc.h>
#include <stdio.h>

#define CAST_LAMBDA(name, from_type, to_type, func_type) ((__perkelang_capture_dummy_##from_type = name, (to_type) {__perkelang_capture_dummy_##from_type.env, (func_type)__perkelang_capture_dummy_##from_type.func}))
#define CALL_LAMBDA0(l, t) (__perkelang_capture_dummy_##t = l, __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env)))
#define CALL_LAMBDA(l, t, ...) (__perkelang_capture_dummy_##t = l, __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env), __VA_ARGS__))


typedef void (*l_char_ptr__vararg_to_void_r)(char*, ...);
typedef int (*l__to_int_r)();
typedef void (*l__to_void_r)();
typedef char* char_ptr;
typedef void (*l_Test_to_void_r)(void*);

struct Test {
    l__to_void_r constructor;
    l__to_void_r f;
};
typedef struct Test* Test;


int main ();
void f ();

void f() {
    printf("f()\n");
}

Test Test_init() {
    Test self = malloc(sizeof(struct Test));
    self->constructor = (l__to_void_r) f;
    self->f = (l__to_void_r) f;

    self->constructor();
    return self;
}

int main() {
    l__to_void_r fu = f;
    fu();
    return 0;
}
