# Ruse

what am i?

## build

```
$ make
$ ./ruse
```

## todo

* rewrite AST for array len/buf
  * put next/prev pointer in AST node to enable in-place rewriting
  * rewrite vardecl = initexpr
  * remove gen_array_buf
* merge unaryexpr and typeexpr parsing
  * for something like `alloc([]int, 42)`, you don't know if the arguments are
    expression or a type until the semantic phase.  So in the parsing stage
    it needs to be parsed into an ambiguous expr kind that can diverge into
    something else later.
* struct codegen
  * size with proper alignment
* arena allocator
* gen if
* strlcpy
* strings
* interop with C
* metaprogramming using macros, e.g. ensuring cycle counters be incremented
    only once in cycle() in simulators
