#include "sema.h"

namespace cmp {

std::string Type::qbe_type_string() const {
  std::string s;
  if (builtin) {
    // TODO: "s", "d", ...
    s = (size == 8) ? "l" : "w";
  } else if (is_pointer()) {
    s = "l";
  } else {
    s = fmt::format(":{}", name->text);
  }
  return s;
}

void QbeGen::emitLoad(QbeValue value, QbeValue addr) {
  emitnl("{} =l loadl {}", value.format(), addr.format());
}

// Codegen for expressions.  'value' denotes whether the caller that contains
// the use of this expression requires the actual value of it, or just the
// address (for lvalues).  If 'value' is true, a handle ID for the generated
// value is pushed to the valstack.
void QbeGen::codegenExprExplicit(Expr *e, bool emit_value) {
  switch (e->kind) {
  case Expr::integer_literal: {
    char cl = (e->type == sema.context.ty_int64) ? 'l' : 'w';
    auto v = stack.make_temp();
    emitnl("{} ={} add 0, {}", v.format(), cl, e->as<IntegerLiteral>()->value);
    annotate("{}: literal", e->loc.line);
    stack.push_temp(v);
    break;
  }
  case Expr::string_literal:
    stack.push_global("string");
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

      stack.push_address_explicit(var->frame_local_id);

      if (emit_value) {
        if (dre->type->size > 8) {
          // FIXME: assumes this is a stack-allocated struct
          assert(!dre->type->builtin);
          assert(dre->type->origin_decl->kind == Decl::struct_);
        } else if (dre->type->size == 8) {
          auto v = stack.make_temp();
          emitLoad(v, stack.pop());
          annotate("{}: load {}", dre->loc.line, dre->text(sema));
          stack.push_temp(v);
        } else if (dre->type->size == 4) {
          auto v = stack.make_temp();
          emitnl("{} =w loadw {}", v.format(), stack.pop().format());
          annotate("{}: load {}", dre->loc.line, dre->text(sema));
          stack.push_temp(v);
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

    assert(c->func_decl->kind == Decl::func);
    auto func_decl = c->func_decl->as<FuncDecl>();

    // codegen arguments first
    std::vector<QbeValue> generated_args;
    for (auto arg : c->args) {
      codegenExprValue(arg);
      generated_args.push_back(stack.pop());
    }

    // Rewrite alloc()'s return type to be a pointer.
    // TODO: generalize to other built-in functions.
    if (func_decl->name == sema.name_table.get("alloc")) {
      assert(func_decl->ret_type->kind == TypeKind::array);
      func_decl->ret_type = get_derived_type(sema, TypeKind::pointer,
                                             func_decl->ret_type->base_type);
      // TODO: w for size is hardcoded; should QbeValue encode type size?
      auto v = stack.make_temp();
      emitnl("{} =l call $malloc(w {}", v.format(), generated_args[0].format());
      emit(")");
      stack.push_temp(v);
    } else if (func_decl->ret_type) {
      auto v = stack.make_temp();
      emitnl("{} ={} call ${}(", v.format(),
             func_decl->ret_type->qbe_type_string(), c->func_decl->name->text);
      for (size_t i = 0; i < c->args.size(); i++) {
        emit("{} {}, ", c->args[i]->type->qbe_type_string(),
             generated_args[i].format());
      }
      emit(")");
      stack.push_temp(v);
    } else {
      emitnl("call ${}(", c->func_decl->name->text);
      // @Copypaste from above
      for (size_t i = 0; i < c->args.size(); i++) {
        emit("{} {}, ", c->args[i]->type->qbe_type_string(),
             generated_args[i].format());
      }
      emit(")");
      // Don't push to valstack here; that the caller doesn't erroneously
      // pop afterwards should have been checked by the semantic phase.
    }
    assert(c->type);
    break;
  }
  case Expr::struct_def: {
    auto sde = e->as<StructDefExpr>();

    // TODO: document why we alloc here
    auto alloc_addr =
        emitStackAlloc(sde->type, sde->loc.line, sde->text(sema));

    // Don't assume anything and just emit all the value copying of each
    // members here.  This might sound expensive, especially in cases where
    // only part of the struct is actually used (e.g. `S {...}.a`).  But
    // figuring out those cases is really a job to be done at a higher IR, not
    // here.

    for (auto term : sde->terms) {
      // Calculate the right offsetted memory location for each
      // member.
      assert(term.field_decl);
      auto addr = stack.make_addr();
      emitnl("{} =l add {}, {}", addr.format(), alloc_addr.format(),
             term.field_decl->offset);
      annotate("{}: offset of {}.{}", sde->loc.line, sde->text(sema),
               term.name->text);
      stack.push_address(addr);
      emitAssignment(term.field_decl, term.initexpr);
    }

    // Leave the address in the valstack; this is what will be used in lieu
    // of the actual value in later nodes.
    stack.push_address(alloc_addr);

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
    codegenExprAddr(mem->parent_expr);

    auto addr = stack.make_addr();
    emitnl("{} =l add {}, {}", addr.format(), stack.pop().format(),
           mem->field_decl->offset);
    annotate("{}: offset of {}", mem->loc.line, mem->text(sema));
    stack.push_address(addr);

    if (emit_value) {
      // TODO: for struct values?
      auto v = stack.make_temp();
      emitnl("{} =w loadw {}", v.format(), stack.pop().format());
      annotate("{}: load {}", mem->loc.line, mem->text(sema));
      stack.push_temp(v);
    }

    break;
  }
  case Expr::subscript: {
    auto se = e->as<SubscriptExpr>();
    codegenExprAddr(se->array_expr);
    codegenExprValue(se->index_expr);
    auto index = stack.pop();
    auto array = stack.pop();

    auto offset = stack.make_temp();
    emitnl("{} =l mul {}, {}", offset.format(), index.format(),
           se->array_expr->type->base_type->size);
    annotate("{}: compute offset from '{}'", se->loc.line,
             se->array_expr->decl->name->text);

    auto buf_field = stack.make_temp();
    auto heap_base = stack.make_temp();
    emitnl("{} =l add {}, {}", buf_field.format(), array.format(),
           array_struct_buf_offset);
    annotate("{}: buf ptr of array '{}'", se->loc.line,
             se->array_expr->decl->name->text);
    emitLoad(heap_base, buf_field);
    annotate("{}: load buf heap address of array '{}'", se->loc.line,
             se->array_expr->decl->name->text);

    auto element = stack.make_temp();
    emitnl("{} =l add {}, {}", element.format(), heap_base.format(),
           offset.format());
    annotate("{}: element address in '{}'", se->loc.line,
             se->array_expr->decl->name->text);
    stack.push_temp(element);

    if (emit_value) {
      auto element_value = stack.make_temp();
      emitnl("{} =w loadw {}", element_value.format(), stack.pop().format());
      annotate("{}: load element in '{}'", se->loc.line,
               se->array_expr->decl->name->text);
      stack.push_temp(element_value);
    }
    break;
  }
  case Expr::unary: {
    auto ue = e->as<UnaryExpr>();
    if (ue->kind == UnaryExpr::ref) {
      codegenExprExplicit(ue->operand, false);
    } else if (ue->kind == UnaryExpr::deref) {
      // Output address of the "*p", which is the value of "p".  This is an
      // address but will be marked as a value in QbeValue.
      codegenExprExplicit(ue->operand, true);
      if (emit_value) {
        // If `value` is true, emit another load to get the
        // dereferenced value.
        // FIXME: size?
        auto v = stack.make_temp();
        emitnl("{} =w loadw {}", v.format(), stack.pop().format());
        annotate("{}: dereference {}", ue->loc.line, ue->operand->text(sema));
        stack.push_temp(v);
      }
      // Otherwise, the address that the pointer contains is on the
      // valstack and we're done.
    } else {
      codegenExprExplicit(ue->operand, emit_value);
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

    auto v = stack.make_temp();
    emitnl("{} =w {} {}, {}", v.format(), op_str, stack.pop().format(),
           stack.pop().format());
    annotate("{}: binary op '{}'", binary->loc.line, binary->op.str());
    stack.push_temp(v);
    break;
  }
  default:
    assert(!"unknown expr kind");
  }
}

void QbeGen::codegenExprValue(Expr *e) { codegenExprExplicit(e, true); }

void QbeGen::codegenExprAddr(Expr *e) { codegenExprExplicit(e, false); }

void QbeGen::codegenStmt(Stmt *s) {
  switch (s->kind) {
  case Stmt::expr:
    codegenExprValue(s->as<ExprStmt>()->expr);
    break;
  case Stmt::decl:
    codegenDecl(s->as<DeclStmt>()->decl);
    break;
  case Stmt::assign: {
    auto as = s->as<AssignStmt>();

    codegenExprAddr(as->lhs);
    // auto rhs_val_id = valstack.pop().id;
    // codegenExpr(as->rhs);

    emitAssignment(as->lhs->decl, as->rhs);

    // This assert doesn't work for pointer dereferences: their value is
    // the target address of this assignment, but they are designated as
    // ValueKind::value.
    // assert(valstack.peek().kind == ValueKind::address);
    // auto lhs_address = valstack.pop();

    // emitln("storew %_{}, {}", rhs_val_id, lhs_address.format());
    // annotate("{}: assign to {}", as->loc.line, as->lhs->text(sema));

    break;
  }
  case Stmt::return_:
    codegenExprValue(s->as<ReturnStmt>()->expr);

    // Whether the top of the valstack might contain is a value or an
    // address, either is fine, because returning a struct by value
    // (usually) happens by address.
    // assert(valstack.peek().kind == ValueKind::value);

    emitnl("ret {}", stack.pop().format());
    // This is here only to make QBE not complain.  In practice, no
    // instructions after this point should be reachable.
    emit("\n@L{}", label_id);
    label_id++;
    break;
  case Stmt::if_: {
    auto if_stmt = s->as<IfStmt>();
    auto id = ifelse_label_id;
    ifelse_label_id++;
    codegenExprValue(if_stmt->cond);
    emitnl("jnz {}, @if_{}, @else_{}", stack.pop().format(), id, id);
    emit("\n@if_{}", id);
    codegenStmt(if_stmt->if_body);
    emitnl("jmp @fi_{}", id);
    emit("\n@else_{}", id);
    if (if_stmt->else_if_stmt) {
      codegenStmt(if_stmt->else_if_stmt);
    } else if (if_stmt->else_body) {
      codegenStmt(if_stmt->else_body);
    }
    emit("\n@fi_{}", id);
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

void QbeGen::codegenDecl(Decl *d) {
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
    v->frame_local_id =
        emitStackAlloc(v->type, v->loc.line, v->name->text).id;

    if (v->assign_expr) {
      stack.push_address_explicit(v->frame_local_id);
      emitAssignment(v, v->assign_expr);
    }

    break;
  }
  case Decl::func: {
    auto f = d->as<FuncDecl>();
    // extern funcs do not need any codegen.
    if (f->extern_) {
      break;
    }

    emit("\nexport function {} ${}(", f->ret_type->qbe_type_string(),
         f->name->text);

    for (auto param : f->params) {
      emit("{} %{}, ", param->type->qbe_type_string(), param->name->text);
    }

    emit(") {{");
    emit("\n@start");

    sema.context.func_stack.push_back(f);
    {
      IndentBlock ib{*this};

      // emit parameters
      for (auto param : f->params) {
        // FIXME: is there a cleaner, centralized way to allocate
        // frame_local_id?
        auto addr = stack.make_addr();
        param->frame_local_id = addr.id;
        emitnl("{} =l add 0, %{}", addr.format(), param->name->text);
      }

      // TODO: Maybe better to just check f->extern_ instead of f->body.
      if (f->body) {
        for (auto line : f->body->stmts) {
          codegen(line);
        }
      }
      // Analyses in the earlier passes should make sure that this ret is
      // not reachable.  This is only here to make QBE work meanwhile
      // those analyses are not fully implemented yet.
      emitnl("ret");
    }
    sema.context.func_stack.pop_back();

    emit("\n}}");
    emit("\n");
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

    emitnl("type :{} = {{", s->name->text);
    for (auto field : s->fields) {
      (void)field;
      emit("w, ");
    }
    emit("}}");
    emit("");
    break;
  }
  default:
    assert(!"unknown decl kind");
  }
}

// Emit a memory-to-memory value copy.
// The memory address of the LHS and value of the RHS is assumed to be already
// pushed onto the valstack. These memory copies may be reduced to register
// operations by QBE.
//
// TODO: what about a = 3?
//
// a = 3
// a = b
// a = S {.a=...}
// S {.a=...}.a
// f(S {.a=...})
void QbeGen::emitAssignment(const Decl *lhs, Expr *rhs) {
  assert(lhs);
  // 'lhs' is already pushed to the stack
  codegenExprValue(rhs);

  // NOTE: rhs_value might not actually have ValueKind::value, e.g. for
  // structs allocated on the stack.
  auto rhs_value = stack.pop();
  auto lhs_addr = stack.pop();
  // This doens't really work, for example for *p = 42 where the address of the
  // LHS is a value of "p".
  // assert(lhs_addr.kind == QbeValue::address);

  if (lhs->type->kind == TypeKind::array) {
    if (rhs->kind == Expr::call &&
        rhs->as<CallExpr>()->func_decl == sema.context.fn_alloc) {
      auto addr = stack.make_addr();
      emitnl("{} =l add {}, {}", addr.format(), lhs_addr.format(),
             array_struct_buf_offset);
      annotate("{}: buf ptr of array '{}'", rhs->loc.line, lhs->name->text);
      stack.push_address(addr);
      lhs_addr = stack.pop();
      // TODO: clean this up by using recursive call to emit_assignment()
      emitnl("storel {}, {}", rhs_value.format(), lhs_addr.format());
    } else {
      assert(!"this assignment to array is not handled");
    }
  }
  // For structs, copy every field one by one.
  // XXX: This assumes that any LHS type that is larger than an eightbyte is
  // a struct.
  else if (lhs->type->size > 8) {
    assert(lhs->type->kind == TypeKind::atom);
    assert(!lhs->type->builtin);
    assert(lhs->type->origin_decl->kind == Decl::struct_);
    auto struct_decl = lhs->type->origin_decl->as<StructDecl>();

    for (auto field : struct_decl->fields) {
      auto rhs_text = fmt::format("{}.{}", rhs->text(sema), field->name->text);

      // calculate address of the field of RHS
      auto rhs_field_addr = stack.make_addr();
      emitnl("{} =l add {}, {}", rhs_field_addr.format(), rhs_value.format(),
             field->offset);
      annotate("{}: address of {}", rhs->loc.line, rhs_text);

      // load value of the field of RHS
      auto field_being_copied = stack.make_temp();
      if (struct_decl->alignment == 8) {
        emitLoad(field_being_copied, rhs_field_addr);
      } else if (struct_decl->alignment == 4) {
        emitnl("{} =w loadw {}", field_being_copied.format(),
               rhs_field_addr.format());
      } else {
        assert(!"unknown alignment");
      }
      annotate("{}: load {}", rhs->loc.line, rhs_text);

      // calculate address of the field of LHS
      auto lhs_text = fmt::format("{}.{}", lhs->name->text, field->name->text);
      auto lhs_field_addr = stack.make_addr();
      emitnl("{} =l add {}, {}", lhs_field_addr.format(), lhs_addr.format(),
             field->offset);
      annotate("{}: address of {}", rhs->loc.line, lhs_text);

      // store value to the field of LHS
      if (struct_decl->alignment == 8) {
        emitnl("storel {}, {}", field_being_copied.format(),
               lhs_field_addr.format());
      } else if (struct_decl->alignment == 4) {
        emitnl("storew {}, {}", field_being_copied.format(),
               lhs_field_addr.format());
      } else {
        assert(!"unknown alignment");
      }
      annotate("{}: store to '{}'", rhs->loc.line, lhs_text);
    }
  }
  // For non-struct types, a simple store is enough.
  else if (lhs->type->size == 8) {
    emitnl("storel {}, {}", rhs_value.format(), lhs_addr.format());
  } else if (lhs->type->size == 4) {
    emitnl("storew {}, {}", rhs_value.format(), lhs_addr.format());
  } else {
    assert(!"unknown type size");
  }

  // Annotate on what we assigned to.
  std::string lhs_text = "LHS";
  if (lhs->is<VarDecl>() && lhs->as<VarDecl>()->name) {
    lhs_text = lhs->as<VarDecl>()->name->text;
  }
  // TODO: annotate proper name for temp decls, e.g. "*p"
  annotate("{}: store to {}", rhs->loc.line, lhs_text);
}

// Emit an address by allocating it on the stack memory.
QbeValue QbeGen::emitStackAlloc(const Type *type, size_t line,
                                  std::string_view text) {
  assert(!sema.context.func_stack.empty());
  assert(type->size > 0);

  auto addr = stack.make_addr();
  emitnl("{} =l ", addr.format());
  // FIXME: unify 'ptr' and 'ref'
  if (type->kind == TypeKind::pointer) {
    // assumes pointers are always 8 bytes
    emit("alloc8");
  } else if (type->kind == TypeKind::array) {
    // FIXME: stack allocate for array types
    emit("alloc8");
  } else if (type->builtin) {
    assert(type->kind != TypeKind::pointer &&
           "ptr & builtin for Types is possible?");
    emit("alloc4");
  } else {
    assert(type->kind == TypeKind::atom);
    assert(type->origin_decl->kind == Decl::struct_ &&
           "non-struct value type?");
    if (type->origin_decl->as<StructDecl>()->alignment == 4) {
      emit("alloc4");
    } else {
      emit("alloc8");
    }
  }
  emit(" {}", type->size);
  annotate("{}: alloc '{}'", line, text);

  return addr;
}

void QbeGen::codegenDataSection() {
  emitnl("data $string = {{ b \"hello\", b 0 }}");
}

void QbeGen::codegen(AstNode *n) {
  switch (n->kind) {
  case AstNode::file: {
    codegenDataSection();
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
