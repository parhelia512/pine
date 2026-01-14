# Pine
- NOTE: this language is still in development. expect bugs as features<br>

## Description
Pine is a statically typed compiled programming language with manual memory management.<br>
It's an ergonomic middle ground between C and C++, similar to new programming languages in this space.<br>

## Principles
Pine's main principle is to seamlessly interop with C and bring modern features while remaining simple to use.

## Features
- Generics
- UTF-8 Strings
- Defer Statements
- Options and Errors (as values)
- Name First Declaration
- Receiver Methods
- Compile Time Execution
- Default Function Arguments
- Allocators

```odin
vec2 :: struct(T: type) {
    x: T;
    y: T;
}

vec2($T).add :: fn(*self, other: vec2(T)) void {
    self.x += other.x;
    self.y += other.y;
}

main :: fn() void {
    pos := vec2(i32){10, 15};
    other := vec2(i32){20, 10};
    pos.add(other);

    option: ?i32 = 10;
    if (option) [op] {
        println("%", op);
    }

    nums := [5]i32{1, 2, 3, 4, 5};
    for (nums) [n] {
        println("%", n);
    }
}
```

## Why not Odin, Zig, ...?
Well, mainly because no language can have all the features I want in a statically compiled manual memory managed language.
- NOTE: I'm not saying other languages are bad for not having these features. There are reasons why they don't have said features and that's respectable

### Features Odin Doesn't Have
- <del>Generics</del>
- First Class Error Unions
- Receiver Methods
- Compile Time Execution
- Immutable Variables

<del>It might be good to point out that Odin has polymorphic parameters, not generics. So `Maybe(vec2(i32))` does not work. Of course there are ways around this but I want true generics</del> As of odin dev-2025-01:6572a52a8, `Maybe(vec2(i32))` works and other generic related things<br>
When I say "immutable variables", I mean "const" variables in the traditional sense. Odin does have constants but they must be compile time known, I think a language should have a way to declare both constant variables and compile time known constants

### Features Zig Doesn't Have
- Default Function Parameters
- Polymorphic Parameters
- Nameless Struct Literal Members
- Receiver Methods
- Not Being a Pain in the Ass

While Zig does have generics, if you want `fn max(x: $T, y: T) T`, this doesn't work. You'd have to pass an `anytype` <del>which could mean that `x` and `y` are different types unless checked inside the function</del>(edit: you could make y be `@TypeOf(x)`) or do `fn max(comptime T: type, x: T, y: T) T`<br>
Zig is also just a pain at times. Unused variables errors, variable that isn't mutated errors, and so on. I'm sure in critical applications this would be useful but for prototyping, it sucks.

## Noteworthy Differences from Mainstream Languages
1. There is no garbage collection
    - Like in languages such as C, you'll have to free memory yourself
1. There are no classes, just structs
    - The key difference is that all fields are public
1. Zig-like Try Catch
    - Functions can give errors as values which need to be handled or by exiting out of the process
1. Name focused declarations
    - This is mainly stylistic but it does have the benefit of grepping for declarations easier such as 'name :: fn('
1. Receiever methods
    - Lets the developer add onto types for more functionality rather than being locked into what's available by the developer of the type
1. Two pointer symbols
    - `*` is a pointer to a variable. `^` is a pointer to a constant. More on why <a href="#why-two-pointer-symbols">here</a>
1. Global constants are compile time constants
    - Constants in global scope are known at compile time and are similar to `#define` or `constexpr`
1. No operator overloading
    - Usually used for math so we'll have common math structures be first class citizens
1. No function overloading
    - Feel as tho it's unnecessary in a language like this but my opinion may change

## Why Two Pointer Symbols
I have noticed one major problem with the ":=", "::" syntax. While it's clear to see which is a variable and which is a constant, there's no way to tell if a pointer points to a variable or constant<br>
In Odin, you can't take the address of a constant because they are compile time known, which means they won't have a runtime address.<br>
Similarly in Golang, you can't take the address as well because a constant may or may not have a runtime address and Golang wants to keep all constants immutable by not letting them be addressable.<br>
<br>
I thought about making constants in Pine only compile time values but this feels restrictive. It would not be possible to have immutable non-compile time known values. Perhaps I'm confusing constants with immutables, but I want compile time constants, constants (immutables), and variables. So instead, you can have a pointer to immutable data and mutable data. Hence, two pointer symbols.<br>
`*` is a pointer to a variable / mutable data.<br>
`^` is a pointer to a constant / immutable data.<br>

## How to build
GCC or Clang, and Make are the only external dependencies needed to build.

### Building on Linux / Unix and Windows
```console
$ make
```

## How to use
GCC or Clang is needed as an external dependency.

`./pine help` shows the commands available. Doing `./pine <command> help` will give more detail into a command.<br>
`./pine build <file.pine>` will build an executable from said file.<br>
`./pine run <file.pine>` will build and run the executable from said file.<br>
<br>
To find documentation or examples, there are folders `docs` and `examples` to help.
