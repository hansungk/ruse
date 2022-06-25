# Ruse

what am i?

## build

```
$ make
$ ./ruse
```

## todo

* merge unaryexpr and typeexpr parsing
  * for something like `alloc([]int, 42)`, you don't know if the arguments are
    expression or a type until the semantic phase.  So in the parsing stage
    it needs to be parsed into an ambiguous expr kind that can diverge into
    something else later.
* annotate generated qbe code
* struct codegen
  * size with proper alignment
* array type
  * make built-in struct for arrays
  * proper allocation with malloc
* unify emit of loads and stores
* arena allocator
* gen if
* strlcpy
* strings
* interop with C
* metaprogramming using macros, e.g. ensuring cycle counters be incremented
    only once in cycle() in simulators
