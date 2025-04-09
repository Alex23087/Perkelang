

#define CAST_LAMBDA(name, from_type, to_type, func_type) ((__perkelang_capture_dummy_##from_type = name, (to_type) {__perkelang_capture_dummy_##from_type.env, (func_type)__perkelang_capture_dummy_##from_type.func}))
#define CALL_LAMBDA0(l, t) (__perkelang_capture_dummy_##t = l, __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env)))
#define CALL_LAMBDA(l, t, ...) (__perkelang_capture_dummy_##t = l, __perkelang_capture_dummy_##t.func(&(__perkelang_capture_dummy_##t.env), __VA_ARGS__))


typedef void (*l_void_ptr_to_void_r)(void*);
typedef void* void_ptr;
typedef int (*l__to_int_r)();
typedef struct env_ {;} env_;
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
    l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_ f = {{}, (l_void_ptr__l_void_ptr_to_void_r_env__to_void_r) __perkelang_lambda_0};
    CALL_LAMBDA(f, l_void_ptr__l_void_ptr_to_void_r_env__to_void_r_env_, ((l_void_ptr_to_void_r_env_){{}, (l_void_ptr_to_void_r) __perkelang_lambda_1}));
    return 0;
}

static void __perkelang_lambda_1(void* __env) {

}

static void __perkelang_lambda_0(void* __env, l_void_ptr_to_void_r_env_ fu) {
    CALL_LAMBDA0(fu, l_void_ptr_to_void_r_env_);
}
