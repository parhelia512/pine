# Keywords
List of all keywords and how to use them.<br>
NOTE: if you are looking for types, look at <a href="./Types.md">Types.md</a>.

```
fn
extern
struct
enum
return
continue
break
fall
true
false
null
if
else
switch
case
for
and
or
defer
cast
sizeof
```

### Legend
In case you are confused with the symbols used in examples / syntax:<br>
Note that there will be contrived examples below the syntax as well as comments if more explanation is needed.<br>
`<_>` means an identifier or pattern that must be there where `_` is the identifier or pattern. Most of the time the `_` will be replaced with a word in relation with the keyword.<br> `[_]` means
`[<_>]` means an optional pattern, but if given, must follow the pattern inside the `<>`.<br>
`...` means there can be more of the upcoming pattern / legend, but it is not a must.<br>
If there is no legend, assume that the example is the only way to use the keyword.<br>

## fn
Used to create a function.
```
<name> :: fn(...[<param_name: param_type [<= param_default_value>]>,]) <return_type> {
    // function body
}

main :: fn() void {

}

main :: fn(args: []string) void {

}

add :: fn(x: i32, y: i32 = 0) i32 {

}
```

## extern
Used to inform the compiler of an external function. By default, an external function is assumed to use the C Calling Convention.
```
extern <name> :: fn(...[<param_name: param_type>  [<= param_default_value>]>,]) <return_type>;

extern print_hello_world :: fn() void;
extern puts :: fn(s: cstring) i32;
extern add :: fn(x: i32, y: i32 = 0) i32;
```

## struct
Used to create a structure.
```
<name> :: struct {
    ...[<field_name: field_type;>]
}

Opaque :: struct {}

Point :: struct {
    x: i32;
    y: i32;
}
```

## enum
Used to create an enumeration.
```
<name> :: enum {
    ...[<field_name;>]
}

Day :: enum {
    Mon;
    Tue;
    // so on
}
```

## return
Return a value from a function.
```
return [<value>];

return;
return 10;
return some_value;
```

## continue
Continue to the next iteration in a loop.
```
continue;
```

## break
Break out of the current loop.
```
break;
```

## fall
Fall to the next case in a switch statement. Must be at the end of a case block.
```
fall;
```

## true
Boolean true value.
```
true
```

## false
Boolean false value.
```
false
```

## null
Optional null value.
```
null
```

## if
Used to branch if a condition is true. Note that `[]` is valid syntax after `()` in an if statement to capture an optional value if it exists.
```
if (<condition>) [[<capture>]] {

} [<else> ...if (<condition>) [[<capture>]]] {

}

if (true) {

} else {

}

if (x == 10) {

} else if (x == 9) {

} else {

}

if (optional_number) [number] {
    // if `optional_number` is of type `?i32`, `number` is of type `i32`. `number` is only available within this scope.
}
```

## else
After an if statement, else is used if the previous condition was not true. else can be followed by another if.<br>
Look at the `if` example above to see `else`.

## switch
Used to switch on a value by different expressions.<br>
The `case` keyword can only come after a switch statement or case statement.<br>
The `else` keyword must be used in a switch statement as a default branch.
```
switch (<value>)
...[case <value> {

}] else {

}

switch (day)
case Day.Mon {
    // monday body
} case Day.Tue {
    // tuesday body
} else {
    // rest of the days body
}
```

## case
See the above `switch` keyword explanation.

## for
For loop as seen in C as well as an iterative for loop (for each). Note that the iterative for loop uses `[]` as part of the syntax.
```
for ([<decl_name: decl_type = decl_value>]; <condition>; [<statement>;]) {
    // body
}
for (<iterator>) [<capture> [<, index>]] {
    // here the outer `[]` is mandatory, as well as the name of the capture.
    // the `index` is optional but must have a `,` before naming the index.
}

for (i: usize = 0; i < large_num; i += 1;) {
    // `i` is only available within this scope.
}

for (; i > 0; i -= 1;) {
    // `i` has already been declared.
}

for (; i > 0;) {
}

for (numbers) [number] {
    // if `numbers` is of type `[5]i32`, `number` is of type `i32`.
}

for (numbers) [number, i] {
    // `i` is of type `usize`
}
```

## and
Boolean and. Note that this *is* short circuiting.
```
<boolean_expression> and <boolean_expression>
```

## or
Boolean or. Note that this *is* short circuiting.
```
<boolean_expression> or <boolean_expression>
```

## defer
Used to defer a statement to the end of a scope. Note that branching keywords such as `return`, `break`, `continue`, `fall` cannot be defered.
```
defer <statement>

defer puts("hello world");

defer {
    // body
}

defer if (x == 10) {
    // body
}
```

## cast
Used to change a value's type to another type, changing the bits in casing as such integer <-> float casting.
```
cast(<type>)<expression>

x_as_float := cast(f32)x;
```

## sizeof
Used to get the size of an expression in bytes. Types are also considered as expressions.<br>
Note that the type `sizeof` "returns" is `usize`.
```
sizeof(<expression>)

x := sizeof(i32);

arr := malloc(sizeof(i32) * 10);
```
