# The jank programming language [![Build Status](https://travis-ci.org/jeaye/jank.svg?branch=master)](https://travis-ci.org/jeaye/jank)

jank is a compiled functional programming language with a strong, static type system, scope-based resource management (RAII), a direct focus on generic, compile-time metaprogramming using both a powerful type-based template system, and hygienic, code-as-data macros.

With a focus on safe concurrency, jank has immutable, persistent data structures and requires mutation to be both explicit and atomic.

## Appetizer
```lisp
(; Update all entities. ;)
(ƒ update (delta real entities vector : (entity)) (Ɐ)
  (map (partial update delta) entities))

(; Damage nearby entities. ;)
(ƒ cast-aoe (area real entities vector : (entity)) (Ɐ)
  (map damage
       (filter (partial within-distance area) (entity))))

(; Find a winner, based on score. ;)
(ƒ find-winner (entities vector : (entity)) (Ɐ)
  (reduce
    (λ (a Ɐ b Ɐ) (Ɐ)
      (if (> (.score a) (.score b))
        a b))
    entities))
```

## Built-in types
There are a few primitive types which are part of the language.

|Name               |Description                                |
|:------------------|:------------------------------------------|
|`boolean`          |A variant of true or false                 |
|`integer`          |A 64bit signed integer                     |
|`real`             |A 64bit float                              |
|`string`           |An array of UTF-8 characters               |

## Functions
```lisp
(ƒ square (i integer) (Ɐ)
  (* i i))
```
Functions are defined via the `function` (or `ƒ`) special identifier and require a `name` identifier, an argument list (which may be empty), and a return type list (which may be empty). Return type lists may also be `(auto)` or `(Ɐ)`, which forces the compiler to deduce the type.

## Structs
```lisp
(struct coord
  (x float)
  (y float))
```
User-defined data types are supported, in the form of structs. Structs may contain any number of members, all of which are public (as in C). Structs may also be generic. Structs may not have member functions. Instead, functions should be designed in a generic manner and may be overloaded/specialized for certain types. See [generics](#generics).

```lisp
(struct name
  (first string "John")
  (last string "Doe"))
```
Struct members may be given a default value. If a member doesn't have a default value, one must be provided at the time of initialization; the compiler will make sure no members are uninitialized.

### Members
Members of struct are accessed with a `.foo` syntax, where `.foo` is a function and `foo` is the field. An example:
```lisp
(struct person
  (first-name string)
  (last-name string))

(print (.last-name john))
```

## Bindings (constant values)
```lisp
(bind name T value)
```
Bindings are defined via the `bind` special identifier and require a `name` identifier, an optional type, and a value. The type may be left out and it will be deduced by the value.

## Generics
Definitions may be dependent on types. Such definitions may be functions or structs. The type list must never be empty. Dependent (incomplete) types of a generic item must be prefixed with `:` to disambiguate from full specializations. To aid in cleanliness, function parameters and return types may be set to `auto` or `Ɐ`, implicitly making them generic.

### Examples
#### Function
```lisp
(; Generic. ;)
(ƒ show : (:T) (o T) ()
  (print o))

(; Short-hand for above, where T isn't needed. ;)
(ƒ show (o auto) ()
  (print o))

(; Full specialization. ;)
(ƒ show : (real) (o real) ()
  (print "real: " o))

(; Partial specialization. ;)
(ƒ show : (coord : (:T-x :T-y)) (o coord : (T-x T-y)) ()
  (print "coord: " o))

(; Non-type parameter partial specialization. ;)
(ƒ show : ((o coord : (:T-x :T-y))) () ()
  (print "coord: " o))

(; Non-type parameter full specialization. ;)
(ƒ show : ((o coord : (real integer))) () ()
  (print "coord: " o))
```
#### Struct
```lisp
(struct coord : (:T-x :T-y)
  (x T-x)
  (y T-y))
```

### Variadics
Generic functions and types can be variadic, allowing any number of parameters, both type and non-type. To accept arbitrary arguments into a tuple, the tuple must be prefixed with `&`; there may only be one such tuple per argument list.

#### Function
```lisp
(ƒ shout (&noises) ()
  (for ((noise noises))
    (print (upper noise))))

(shout "fus" " ro" " dah")
(; Prints => FUS RO DAH ;)
```

#### Struct
```lisp
(; Use variadic type args as a form of policies. ;)
(struct coord : (:T-component &T-policies)
  (x T-component)
  (y T-component))

(; Builds a cartesian coordinate with an offset origin.
 ; All of this is built in to the coordinate's type. ;)
(coord : (real cartesian origin : (77.0)) 0.0 0.0)
```

## Comments
Only multi-line comments are supported. Anything within `(;` and `;)` is considered a comment. Nested comments are allowed. Each closing `;)` matches the closest opening `(;`, even overpowering previous closing `;)` along the way. Thus, `(; foo ;) ;)` is a single comment.

## Resource management
```lisp
(ƒ construct (...) (Ɐ)
  ...)

(ƒ destruct (o Ɐ) ()
  ...)
```
Scope-based resource management ties resource ownership to object lifetimes. Types can take advantage of this by specializing `construct` and `destruct` to perform any custom logic.

To construct an object using a constructor, `new` or `construct` must be called. To construct an object using aggregate initialization, the type of the object can be used as the function; all members which don't have defaults provided in the `struct` definition must be specified in aggregate initialization.

`new` is a convenience macro which will first try to match constructors and will fall back on aggregate initialization. These checks are all done at compile-time. Since `new` allows types to intercept aggregate initialization with constructors, it's the preferred way of instantiating objects.

Since constructors are the functions to actually create objects, not something that's called after creation, delegation to other constructors and other functions is very flexible.

#### Example
```lisp
(struct coord : (:T-x :T-y)
  (x T-x)
  (y T-y))

(; Defines a constructor which has a side effect and then uses
 ; aggregate initialization to build the coord. ;)
(ƒ construct : (coord : (:T-x :T-y)) (x T-x y T-y) (Ɐ)
  (print "constructing object")
  (coord : (T-x T-y) x y))

(ƒ destruct : (:T-x :T-y) (c coord : (T-x T-y)) ()
  (print "destructing coord"))

(; Calls the constructor explicitly. ;)
(bind c1 (construct : coord : (real real) 0.0 5.4))

(; Calls the constructor via new. ;)
(bind c2 (new : coord : (real real) 0.0 5.4))
```

## Type aliasing
All type aliases are strong. Since the focus is so strongly on generics, types are designed to be specialized and aliased to create unique, custom types. Aliases can also be generic.

### Examples
```lisp
(; name is now a strong type alias of the builtin string type. ;)
(alias string as name)

(; position is generic, yet still strong. ;)
(alias coord : (:T-x :T-y) as position : (T-x T-y))
```

## Generic constraints
Constraints can be applied to various definitions, including functions and structs, using the optional `where` expression. The expression acts along with overload resolution to further exclude instantiations/matches. The expression must evaluate to boolean and can use any functions, macros, and values available at compile-time.

### Examples
#### Functions
```lisp
(; Specialize on generic macros as type traits. ;)
(macro ^number? : (:T) ()
  false)
(macro ^number? : (integer) ()
  true)
(macro ^number? : (real) ()
  true)

(ƒ square : (:T) (i T) (Ɐ) where (^number? : T)
  (* i i))
```
#### Structs
```lisp
(struct coord : (:T) where (^number? : T)
  (data T))
```

## Enums
Enums function as variant sum types; each variant can have its own type or simply represent its own value (as in C). Enums can also be generic. Value enums work similar to C, whereas type enums must use matching to destructure.
```lisp
(; Unique values, like a C enum. ;)
(enum gender
  male
  female
  other)

(; Unique types. ;)
(enum character
  (struct digit
    (value integer))
  (struct other))

(; A generic enum of unique types. ;)
(enum optional : (:T)
  (struct some
    (value T))
  (struct none))
```

## Branching
```lisp
(bind num 42)

(if (even? num)
  (print "even")
  (print "not even"))
```
Branching, using `if`, allows for specifying a single form for the true and false cases. All conditions must be of type `boolean` and the false case is optional. To have more than one line in a true or false case, introduce scope with a `do` statement.

```lisp
(ƒ next-even (i integer) (Ɐ)
  (if (even? i)
    (do
      (print "even")
      (+ 2 i))
    (do
      (print "not even")
      (+ 1 i))))
```

### Expressions
`if` and `do` statements can be used as expressions in function calls, allowing arbitrary code bodies to be used as parameters.

```lisp
(print
  (if (even? 3)
    "even"
    "odd"))

(print
  (do
    "always true"))
```

## Macros
Macros provide the ability for arbitrary code execution, including disk and network IO, and direct modification of the source code at compile-time. Macros, like functions, can be made generic and can be partially and fully specialized. Along with generics, macros use the same type-safety and overloading rules as normal functions. There are two added types, during macro definition, which can be used: `^list` and `^atom` which correspond to arbitrary lists of code and single code atoms respectively.

The form of a macro definition is very similar to that of a function definition. Macros, however, have no specific return type; they emit whatever they return as replacement code for the call. As a convention, macros begin with `^`.

### Non-generic
```lisp
(macro ^reverse-args (args ^list)
  (list (first args) (reverse (rest args))))

(^reverse-args (print 3 2 1))
(; Becomes => (print 1 2 3) at compile-time. ;)

(macro ^constructor (type ^list args ^list &body)
  (ƒ construct : type args (Ɐ)
    body))

(^constructor (person) (first-name Ɐ last-name Ɐ)
  (person first-name last-name))
```

### Generic
```lisp
(; Compile-time type traits using partial specialization. ;)
(macro ^sequence? : (:T) ()
  false)
(macro ^sequence? : (list : (:T)) ()
  true)
(macro ^sequence? : (vector : (:T)) ()
  true)
```

## Strings
For now, due to the simplicity of the parser, parentheses in strings need to be escaped. This will change, but it's very low priority.

## Native API
Native types and functions can be exposed through the native API, which is in C++14. Though currently only exposed in the compiler, there's planned support for loading dynamic libraries *at compile-time* which specify wrappers around native code. An example might be OpenGL, glfw, or other similar packages.

## Building
To build, follow the normal steps:
```bash
$ mkdir build ; cd build
$ cmake ..
$ make -j8
```

## Tests
There is currently a suite of tests outlining most functionality. The tests are implemented using [jest](https://github.com/jeaye/jest) - a sane and minimal C++14 testing framework. You can test with (from the build directory):
```bash
$ make jank_test
```

## Progress
|Compiler                     |Status                                     |
|:----------------------------|:------------------------------------------|
|non-generic functions        |Complete and tested.                       |
|non-generic bindings         |Complete and tested.                       |
|non-generic native functions |Complete and tested.                       |
|if/do statements             |Complete and tested.                       |
|if/do expressions            |Complete and tested.                       |
|lambda expressions           |Complete and tested.                       |
|first-class functions        |Complete and tested.                       |

|Interpreter                  |Status                                     |
|:----------------------------|:------------------------------------------|
|non-generic functions        |Complete and tested.                       |
|non-generic bindings         |Complete and tested.                       |
|non-generic native functions |Complete and tested.                       |
|arithmetic for primitives    |Complete and tested.                       |
|if/do statements             |Complete and tested.                       |
|if/do expressions            |Complete and tested.                       |
|lambda expressions           |Complete and tested.                       |
|first-class functions        |Complete and tested.                       |

## Editor support
There are syntax files for Vim available in the `vim` directory of the repository. You can add these to your runtime path using something like:

```viml
set runtimepath^=~/projects/jank/vim
set runtimepath^=~/projects/jank/vim/after
```

## License
jank is under the MIT open-source license.  
See the `LICENSE` file or http://opensource.org/licenses/MIT
