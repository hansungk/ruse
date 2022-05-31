// -*- C++ -*-
#ifndef CMP_TYPES_H
#define CMP_TYPES_H

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <string>
#include <unordered_map>
#include <vector>

namespace cmp {

struct Type;
struct Decl;
struct VarDecl;
struct StructDecl;
struct EnumDecl;
struct FuncDecl;
struct Sema;
struct VarDecl;
struct StructDecl;
struct EnumDecl;
struct FuncDecl;

// 'Name' corresponds to a single unique identifier string in the source text.
// There may be multiple occurrences of a string in the source text, but only
// one instance of the matching Name can reside in the name table.
struct Name {
  const char *text;
};

// 'NameTable' is a hash table of Names queried by their string value.  It
// serves to reduce the number of string hashing operation, since we can look
// up the symbol table using Name instead of raw char * throughout the semantic
// analysis.
struct NameTable {
  Name *push(const char *s) { return pushlen(s, strlen(s)); }
  Name *pushlen(const char *s, size_t len) {
    Name *found = get(std::string{s, len});
    if (found)
      return found;

    Name n{strndup(s, len)};
    auto pair = map.insert({std::string{s, len}, n});
    return &pair.first->second;
  }
  Name *get(const std::string &s) {
    auto found = map.find(s);
    if (found == map.end()) {
      return nullptr;
    } else {
      return &found->second;
    }
  }
  ~NameTable() {
    for (auto &m : map) {
      free((void *)m.second.text);
    }
  }
  std::unordered_map<std::string, Name> map;
};

enum class TypeKind {
  value, // built-in, struct
  pointer,
  array,
};

// 'Type' represents a type, whether it be a built-in type, a user-defined
// struct, or a reference to another of those.  Similar to Names, Types are
// designed to exist as singular instances in the lifetime of the compiler, and
// are meant to be compared by a simple pointer comparison.  This is a
// difference with 'Decl', which may exist as many as there are different
// instances of a variable with different memory locations.
//
// Types should be allocated on the heap and stored as a pointer member in AST
// nodes, rather than stored as a by-value member, because their presence may
// outlive the lexical scope of a single AST node. TODO: say about whether
// storing them in memory pools or the scoped table.
struct Type {
  TypeKind kind = TypeKind::value;
  // Name of the type. TODO: include * or [] in the name?
  Name *name = nullptr;
  // Whether this is a builtin type or not.
  bool builtin = false;
  union {
    // For value types: back-reference to the decl that this type
    // originates from.
    Decl *origin_decl = nullptr;
    // For derived types e.g. pointers: the target type that this type
    // refers to.
    Type *referee_type;
  };
  // Memory size of this type in bytes.
  long size = 0;

  Type(TypeKind k, Name *n) : kind(k), name(n) {}

  bool is_struct() const;
  bool is_pointer() const;
  bool is_builtin(Sema &sema) const;
};

Type *make_struct_type(Sema &sema, Name *n, Decl *decl);
Type *make_pointer_type(Sema &sema, Name *name, TypeKind ptr_kind,
                        Type *referee_type);
Type *make_builtin_type(Sema &sema, Name *n);

} // namespace cmp

#endif
