# Ruse

what am i?

## build

```
$ make
$ ./ruse
```

## todo

* struct codegen
  * size with proper alignment
* array type
  * make built-in struct for arrays
  * proper allocation with malloc
* annotate generated qbe code
* unify emit of loads and stores
* arena allocator
* gen if
* strlcpy
* strings
* interop with C
* metaprogramming using macros, e.g. ensuring cycle counters be incremented
    only once in cycle() in simulators
