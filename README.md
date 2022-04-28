# Ruse

what am i?

## build

```
$ make
$ ./ruse
```

## todo

* gen func params
  * find out the current function, and look up param decls
* type instantiation
* gen func and if
* use unions rather than n->children and n->lhs for callexpr
* strlcpy
* cleaner emit of loads and stores
* strings
* interop with C
* metaprogramming using macros, e.g. ensuring cycle counters be incremented
  only once in cycle() in simulators
