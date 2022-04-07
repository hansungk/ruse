#include "sema.h"

namespace cmp {

std::string qbeAbityString(const Type *type) {
  std::string s;
  if (type->builtin) {
    // TODO: "l", "s", "d", ...
    s = "w";
  } else {
    s = std::string{":"} + type->name->text;
  }
  return s;
}

// 'value' denotes whether the caller that contains the use of this expression
// requires the actual value of it, or just the address (for lvalues).  If
// 'value' is true, a handle for the generated value is placed on the valstack
// top.
void QbeGenerator::codegenExprExplicit(Expr *e, bool value) {
  switch (e->kind) {
  case Expr::integer_literal:
    emit("%_{} =w add 0, {}", valstack.next_id,
         e->as<IntegerLiteral>()->value);
    annotate("{}: integer literal", e->loc.line);
    valstack.pushTempValue();
    break;
  case Expr::string_literal:
    assert(!"not implemented");
    break;
  case Expr::decl_ref: {
    auto dre = e->as<DeclRefExpr>();

    // The 'a' in this expression generates a memory load from a stack address:
    //   ... = a
    // But these do not:
    //   ... = a.mem
    //   ... = *a
    //   ... = a[3]
    //
    // Whether a load should be generated or not is determined at the use of
    // the expression.  When we reach the statement that actually uses an
    // expression, we could query for the type of the expression, and only
    // generate load for lvalues or something.  All we have to do for each
    // expression is to get the address ready (if it has one) on the valstack.
    //
    // However, this way we have to do the load/no-load check for all possible
    // positions that can use an expression and it could complicate things.
    // Maybe just paying the price of generating unused loads and pushing the
    // actual value of the expression on the valstack could be much simpler
    // implementation-wise.
    //
    // Outputting values when the expression is converted from an lvalue to an
    // rvalue also sounds good, but this doesn't work considering that
    // expressions may be used without being converted to rvalues in advance.
    if (dre->decl->kind == Decl::var) {
      auto var = dre->decl->as<VarDecl>();

      valstack.pushAddressExplicit(var->frame_local_id);

      if (value) {
        if (dre->type->size > 8) {
          // FIXME: assumes this is a stack-allocated struct
          assert(!dre->type->builtin);
          assert(dre->type->origin_decl->kind == Decl::struct_);
        } else if (dre->type->size == 8) {
          emit("%_{} =l loadl {}", valstack.next_id, valstack.pop().format());
          annotate("{}: load {}", dre->loc.line, dre->text(sema));
          valstack.pushTempValue();
        } else if (dre->type->size == 4) {
          emit("%_{} =w loadw {}", valstack.next_id, valstack.pop().format());
          annotate("{}: load {}", dre->loc.line, dre->text(sema));
          valstack.pushTempValue();
        } else {
          assert(!"unknown alignment");
        }
      }
    } else {
      assert(!"not implemented");
    }
    break;
  }
  case Expr::call: {
    auto c = e->as<CallExpr>();

    assert(c->callee_decl->kind == Decl::func);
    auto func_decl = c->callee_decl->as<FuncDecl>();

    // codegen arguments first
    std::vector<Value> generated_args;
    for (auto arg : c->args) {
      codegenExprExplicit(arg, true);
      generated_args.push_back(valstack.pop());
    }

    if (func_decl->ret_type_expr) {
      emit("%_{} ={} call ${}(", valstack.next_id,
           qbeAbityString(func_decl->ret_type), c->callee_decl->name->text);

      for (size_t i = 0; i < c->args.size(); i++) {
        emitSameline("{} {}, ", qbeAbityString(c->args[i]->type),
                     generated_args[i].format());
      }

      emitSameline(")");

      valstack.pushTempValue();
    } else {
      emit("call ${}(", c->callee_decl->name->text);

      // @Copypaste from above
      for (size_t i = 0; i < c->args.size(); i++) {
        emitSameline("{} {}, ", qbeAbityString(c->args[i]->type),
                     generated_args[i].format());
      }

      emitSameline(")");

      // Don't push to valstack here; that the caller doesn't erroneously
      // pop afterwards should have been checked by the semantic phase.
    }
    assert(c->type);
    break;
  }
  case Expr::struct_def: {
    auto sde = e->as<StructDefExpr>();

    // TODO: document why we alloc here
    auto id = emitStackAlloc(sde->type, sde->loc.line, sde->text(sema));

    // Don't assume anything and just emit all the value copying of each
    // members here.  This might sound expensive, especially in cases where
    // only part of the struct is actually used (e.g. `S {...}.a`).  But
    // figuring out those cases is really a job to be done at a higher IR, not
    // here.

    for (auto term : sde->terms) {
      // Calculate the right offsetted memory location for each
      // member.
      assert(term.field_decl);
      emit("%a{} =l add %a{}, {}", valstack.next_id, id,
           term.field_decl->offset);
      annotate("{}: offset of {}.{}", sde->loc.line, sde->text(sema),
               term.name->text);
      valstack.pushAddress();
      emitAssignment(term.field_decl, term.initexpr);
    }

    // Leave the address in the valstack; this is what will be used in lieu
    // of the actual value in later nodes.
    valstack.pushAddressExplicit(id);

    break;
  }
  case Expr::member: {
    auto mem = e->as<MemberExpr>();

    // We can't do complete code generation at this end without recursing
    // into the child LHS expression because of cases like these:
    //
    //   ... = (*p).memb
    //   ... = f().memb
    //
    // So we have to recurse into things and let the children decide
    // whether they want to emit value or address.

    // emit correct address first
    codegenExprExplicit(mem->parent_expr, false);

    emit("%a{} =l add {}, {}", valstack.next_id, valstack.pop().format(),
         mem->field_decl->offset);
    annotate("{}: offset of {}", mem->loc.line, mem->text(sema));
    valstack.pushAddress();

    if (value) {
      // TODO: for struct values?
      emit("%_{} =w loadw {}", valstack.next_id, valstack.pop().format());
      annotate("{}: load {}", mem->loc.line, mem->text(sema));
      valstack.pushTempValue();
    }

    break;
  }
  case Expr::unary: {
    auto ue = e->as<UnaryExpr>();

    if (ue->kind == UnaryExpr::ref) {
      codegenExprExplicit(ue->operand, false);
    } else if (ue->kind == UnaryExpr::deref) {
      // Output its value first, which is an address.
      codegenExprExplicit(ue->operand, true);
      if (value) {
        // If `value` is true, emit another load to get the
        // dereferenced value.
        // FIXME: size?
        emit("%_{} =w loadw {}", valstack.next_id, valstack.pop().format());
        annotate("{}: dereference {}", ue->loc.line, ue->operand->text(sema));
        valstack.pushTempValue();
      }
      // Otherwise, the address that the pointer contains is on the
      // valstack and we're done.
    } else {
      codegenExprExplicit(ue->operand, value);
    }
    break;
  }
  case Expr::binary: {
    auto binary = e->as<BinaryExpr>();
    codegenExprExplicit(binary->lhs, true);
    codegenExprExplicit(binary->rhs, true);

    const char *op_str = NULL;
    switch (binary->op.kind) {
    case Token::plus:
      op_str = "add";
      break;
    case Token::doubleequals:
      // TODO: not "w"
      op_str = "ceqw";
      break;
    case Token::notequals:
      op_str = "cnew";
      break;
    default:
      assert(!"unknown binary expr kind");
    }
    emit("%_{} =w {} {}, {}", valstack.next_id, op_str, valstack.pop().format(),
         valstack.pop().format());
    annotate("{}: binary op '{}'", binary->loc.line, binary->op.str());
    valstack.pushTempValue();
    break;
  }
  default:
    assert(!"unknown expr kind");
  }
}

void QbeGenerator::codegenExpr(Expr *e) { codegenExprExplicit(e, true); }

void QbeGenerator::codegenExprAddress(Expr *e) {
  codegenExprExplicit(e, false);
}

void QbeGenerator::codegenStmt(Stmt *s) {
  switch (s->kind) {
  case Stmt::expr:
    codegenExpr(s->as<ExprStmt>()->expr);
    break;
  case Stmt::decl:
    codegenDecl(s->as<DeclStmt>()->decl);
    break;
  case Stmt::assign: {
    auto as = s->as<AssignStmt>();

    codegenExpr(as->rhs);
    auto rhs_val_id = valstack.pop().id;

    codegenExprAddress(as->lhs);
    // This assert doesn't work for pointer dereferences: their value is
    // the target address of this assignment, but they are designated as
    // ValueKind::value.
    // assert(valstack.peek().kind == ValueKind::address);
    auto lhs_address = valstack.pop();

    emit("storew %_{}, {}", rhs_val_id, lhs_address.format());
    annotate("{}: assign to {}", as->loc.line, as->lhs->text(sema));
    break;
  }
  case Stmt::return_:
    codegenExpr(s->as<ReturnStmt>()->expr);

    // Whether the top of the valstack might contain is a value or an
    // address, either is fine, because returning a struct by value
    // (usually) happens by address.
    // assert(valstack.peek().kind == ValueKind::value);

    emit("ret {}", valstack.pop().format());
    // This is here only to make QBE not complain.  In practice, no
    // instructions after this point should be reachable.
    emitSameline("\n@L{}", label_id);
    label_id++;
    break;
  case Stmt::if_: {
    auto if_stmt = s->as<IfStmt>();
    auto id = ifelse_label_id;
    ifelse_label_id++;
    codegenExpr(if_stmt->cond);
    emit("jnz {}, @if_{}, @else_{}", valstack.pop().format(), id, id);
    emitSameline("\n@if_{}", id);
    codegenStmt(if_stmt->if_body);
    emit("jmp @fi_{}", id);
    emitSameline("\n@else_{}", id);
    if (if_stmt->else_if_stmt) {
      codegenStmt(if_stmt->else_if_stmt);
    } else if (if_stmt->else_body) {
      codegenStmt(if_stmt->else_body);
    }
    emitSameline("\n@fi_{}", id);
    break;
  }
  case Stmt::compound:
    for (auto line : s->as<CompoundStmt>()->stmts) {
      codegen(line);
    }
    break;
  default:
    assert(!"unknown stmt kind");
  }
}

void QbeGenerator::codegenDecl(Decl *d) {
  switch (d->kind) {
  case Decl::var: {
    auto v = d->as<VarDecl>();

    if (sema.context.func_stack.empty()) {
      assert(!"vardecl outside function?");
    }

    // frame_local_id for vardecls is set on-the-fly during code
    // generation. This is because there are other cases that allocate on
    // the stack (such as returning a large struct by-value) that are not
    // emitted by a vardecl.
    v->frame_local_id = emitStackAlloc(v->type, v->loc.line, v->name->text);

    if (v->assign_expr) {
      valstack.pushAddressExplicit(v->frame_local_id);
      emitAssignment(v, v->assign_expr);
    }

    break;
  }
  case Decl::func: {
    auto f = d->as<FuncDecl>();

    emitSameline("\nexport function {} ${}(", qbeAbityString(f->ret_type),
                 f->name->text);

    for (auto param : f->params) {
      emitSameline("{} %{}, ", qbeAbityString(param->type), param->name->text);
    }

    emitSameline(") {{");
    emitSameline("\n@start");

    sema.context.func_stack.push_back(f);
    {
      QbeGenerator::IndentBlock ib{*this};

      // emit parameters
      for (auto param : f->params) {
        // FIXME: is there a cleaner, centralized way to allocate
        // frame_local_id?
        param->frame_local_id = valstack.next_id;
        valstack.next_id++;
        emit("%a{} =l add 0, %{}", param->frame_local_id, param->name->text);
      }

      for (auto line : f->body->stmts) {
        codegen(line);
      }
      // Analyses in the earlier passes should make sure that this ret is
      // not reachable.  This is only here to make QBE work meanwhile
      // those analyses are not fully implemented yet.
      emit("ret");
    }
    sema.context.func_stack.pop_back();

    emitSameline("\n}}");
    emitSameline("\n");
    break;
  }
  case Decl::struct_: {
    auto s = d->as<StructDecl>();

    long max_field_size = 0;
    for (const auto f : s->fields) {
      if (f->type->size > max_field_size) {
        max_field_size = f->type->size;
      }
    }
    if (max_field_size <= 4) {
      s->alignment = 4;
    } else if (max_field_size <= 8) {
      s->alignment = 8;
    } else {
      assert(!"unimplemented: too large field size for alignment");
    }

    size_t offset = 0;
    for (size_t i = 0; i < s->fields.size(); i++) {
      s->fields[i]->offset = offset;
      offset += s->alignment;
    }
    s->type->size = offset;

    emit("type :{} = {{", s->name->text);
    for (auto field : s->fields) {
      (void)field;
      emitSameline("w, ");
    }
    emitSameline("}}");
    emitSameline("");
    break;
  }
  default:
    assert(!"unknown decl kind");
  }
}

// Emit a memory-to-memory value copy.
// The memory address of the LHS and the value of the RHS are assumed to be
// already pushed onto the valstack.
// These memory copies may be reduced to register operations by QBE.
//
// TODO: what about a = 3?
//
// a = 3
// a = b
// a = S {.a=...}
// S {.a=...}.a
// f(S {.a=...})
void QbeGenerator::emitAssignment(const Decl *lhs, Expr *rhs) {
  codegenExpr(rhs);

  // NOTE: rhs_value might not actually have ValueKind::value, e.g. for
  // structs allocated on the stack.
  auto rhs_value = valstack.pop();
  auto lhs_address = valstack.pop();
  assert(lhs_address.kind == ValueKind::address);

  // For structs, copy every field one by one.
  // XXX: This assumes that any LHS type that is larger than an eightbyte is
  // a struct.
  auto lhs_type = lhs->type;
  if (lhs_type->size > 8) {
    assert(lhs_type->kind == TypeKind::value);
    assert(!lhs_type->builtin);
    assert(lhs_type->origin_decl->kind == Decl::struct_);
    auto struct_decl = lhs_type->origin_decl->as<StructDecl>();

    for (auto field : struct_decl->fields) {
      auto rhs_text = fmt::format("{}.{}", rhs->text(sema), field->name->text);

      // load address from source; calculate address of the field
      emit("%a{} =l add {}, {}", valstack.next_id, rhs_value.format(),
           field->offset);
      annotate("{}: address of {}", rhs->loc.line, rhs_text);
      valstack.pushAddress();

      // load value of the field
      if (struct_decl->alignment == 8) {
        emit("%_{} =l loadl {}", valstack.next_id, valstack.pop().format());
      } else if (struct_decl->alignment == 4) {
        emit("%_{} =w loadw {}", valstack.next_id, valstack.pop().format());
      } else {
        assert(!"unknown alignment");
      }
      annotate("{}: load {}", rhs->loc.line, rhs_text);
      valstack.pushTempValue();

      // calculate the address of the LHS field first
      auto lhs_text = fmt::format("{}.{}", lhs->name->text, field->name->text);
      auto value_to_be_copied = valstack.pop();
      emit("%a{} =l add {}, {}", valstack.next_id, lhs_address.format(),
           field->offset);
      annotate("{}: address of {}", rhs->loc.line, lhs_text);
      valstack.pushAddress();

      // store value to dest address of LHS
      auto lhs_address = valstack.pop();
      if (struct_decl->alignment == 8) {
        emit("storel {}, {}", value_to_be_copied.format(),
             lhs_address.format());
      } else if (struct_decl->alignment == 4) {
        emit("storew {}, {}", value_to_be_copied.format(),
             lhs_address.format());
      } else {
        assert(!"unknown alignment");
      }
      annotate("{}: store to {}", rhs->loc.line, lhs_text);
    }
  }
  // For non-struct types, a simple store is enough.
  else if (lhs_type->size == 8) {
    emit("storel {}, {}", rhs_value.format(), lhs_address.format());
  } else if (lhs_type->size == 4) {
    emit("storew {}, {}", rhs_value.format(), lhs_address.format());
  } else {
    assert(!"unknown type size");
  }
  annotate("{}: store to {}", rhs->loc.line, lhs->name->text);
}

// Emit a value by allocating it on the stack memory.  That value will be
// handled via its address.  `line` and `text` are used for annotations.
long QbeGenerator::emitStackAlloc(const Type *type, size_t line,
                                    std::string_view text) {
  assert(!sema.context.func_stack.empty());
  // auto current_func = context.func_stack.back();
  // long id = current_func->frame_local_id_counter;
  // current_func->frame_local_id_counter++;
  long id = valstack.next_id;
  valstack.next_id++;

  assert(type->size > 0);

  emit("%a{} =l ", id);
  // FIXME: unify 'ptr' and 'ref'
  if (type->kind == TypeKind::ptr || type->kind == TypeKind::ref) {
    // assumes pointers are always 8 bytes
    emitSameline("alloc8");
  } else if (type->builtin) {
    assert(type->kind != TypeKind::ptr &&
           "ptr & builtin for Types is possible?");
    emitSameline("alloc4");
  } else {
    assert(type->kind == TypeKind::value);
    assert(type->origin_decl->kind == Decl::struct_ &&
           "non-struct value type?");
    if (type->origin_decl->as<StructDecl>()->alignment == 4) {
      emitSameline("alloc4");
    } else {
      emitSameline("alloc8");
    }
  }
  emitSameline(" {}", type->size);
  annotate("{}: alloc {}", line, text);

  return id;
}

void QbeGenerator::codegen(AstNode *n) {
  switch (n->kind) {
  case AstNode::file: {
    for (auto toplevel : n->as<File>()->toplevels) {
      codegen(toplevel);
    }
    break;
  }
  case AstNode::stmt:
    codegenStmt(n->as<Stmt>());
    break;
  case AstNode::decl:
    codegenDecl(n->as<Decl>());
    break;
  default:
    assert(!"unknown ast kind");
  }
}

} // namespace cmp
