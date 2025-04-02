typeclass <T> Eq {
    let eq: (T) -> bool,
}

typeclass Eq {
    let eq: void* -> bool;
}

class TestClass: Eq{
    let x: int = 0;

    let eq : TestClass -> bool = (TestClass x) => {
        return this == x
    }
    
    let eq : Eq -> bool = (Eq x) => {
        self.x ++;
        return true;
    }
}


fun f(obj: Eq+Monad, obj2 : Eq): bool {
    return obj.eq(obj2);
}

fun main(): int {
    let obj: TestClass = new TestClass();
    let obj2: TestClass = new TestClass();
    obj.eq((Eq<TestClass>)obj2);
}