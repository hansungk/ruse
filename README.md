# Ruse

what am i?

## build

```
$ make
$ ./ruse
```

## todo

* rewrite AST for array len/buf
  * rewrite `arr = alloc()` to `arr.buf = ...` and `arr.len = ...`
  * rewrite arr[i] to arr.buf[i]?
    * support `ptr[i]` form only at the codegen AST level, not the language
    * this will remove the need for `gen_array_buf` in codegen
  * prev pointer in AST?
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
