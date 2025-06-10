# Perk

Perk is a modern low level programming language, compiled to C. It builds on C's simplicity, adding features such as option types, lambdas and typeclasses. Its current goal is to replace C in the implementation of the MellOS operating system.

---

## Run Perk with nix

```bash
nix run github:Alex23087/Perk -- [files]
```

This will build the `perkc` compiler and run it directly (you can pass any arguments after `--`).

## Features

- Simple **type inference**

```
fun main () : int {
    // No need to write the type here!
    let x := 3;
    return 0;
} 
```

- **Tuples**

```
let position: (float * float) = (200.,200.)
```

- **Option types**, with implicit boxing at assignment

```
    let z : (int*)? = nothing;
    z = just &x;
```

- **Typeclass system** (*Archetypes and Models*)

```
archetype Drawable {
    position : (float * float),
    rotation : float,
    texture : SDL_Texture*,
    size : (float * float),
    draw : SDL -> void
}
```

```
model Character: Drawable, PhysicsObject {
    let position: (float * float) = (200.,200.),
    let velocity: (float * float) = (0., 0.),
    ...

    fun constructor (sdl : SDL) : void {
        ...
    },

    fun draw (sdl : SDL) : void {
        ...
    }
}
```

- Parametric generic functions through archetype sums

```
let f := (x: <Drawable+PhysicsObject>) : void {...}

f(drawable_physics_thing as Drawable+PhysicsObject)
```

- Capturing anonymous functions

```
fun streq (s1 : char*) : char* => int {
    return (s2 : char*) : int {return (strcmp (s1, s2) == 0)}
}
```

- Static subset (only uses the stack, only allows non-capturing lambdas)