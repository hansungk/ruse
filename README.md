# Ruse

what am i?

## build

```
$ make
$ ./ruse
```

## todo

* gen func params
  * fix valstack_push_temp to always take the value as an explicit parameter
* type instantiation
* gen func and if
* use unions rather than n->children and n->lhs for callexpr
* strlcpy
* cleaner emit of loads and stores
* strings
* interop with C
* metaprogramming using macros, e.g. ensuring cycle counters be incremented
  only once in cycle() in simulators
