#include <malloc.h>
#include <stdio.h>

typedef void (*l_char_ptr__vararg_to_void_r)(char*, ...);
typedef int (*l__to_int_r)();
typedef void (*l__to_void_r)();
typedef char* char_ptr;

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
