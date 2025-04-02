typedef int bool;
typedef bool (*void_ptr_to_bool)(void *, void *);

bool eq(void *obj, void *obj2)
{
    return obj == obj2;
}

struct Eq
{
    void_ptr_to_bool eq;
};

struct TestClass
{
    struct eq;
    struct eq2;
    struct eq3;
    struct monad;
};

struct TestClass2
{
    int a;
    void_ptr_to_bool eq;

    struct eq;
    struct eq3;
    struct monad;
};

struct eqplusmonad
{
    struct eq;
    struct monad;
    void *self;
};

(Eq)(banana : Monad + Eq)

    ->

    struct Eq culo;

culo->eq = banana->eq;
culo->self = banana;

bool f(void *obj, eqplusmonad obj_fdsaf, Eq obj2)
{
    return obj_fsdaf->eq(obj, obj2);
}

bool f(TestClass obj, TestClass2 obj2)

    int main()
{
    struct TestClass obj;
    obj.eq = *eq;
    struct TestClass obj2;
    obj2.eq = *eq;

    f(&obj, &(obj.eq), &obj2, &(obj2.eq));
}