#include <stdio.h>
#include <assert.h>
#include "include/stmnts.h"
#include "include/exprs.h"
#include "include/lexer.h"
#include "include/stb_ds.h"

Stmnt stmnt_none(void) {
    return (Stmnt){
        .kind = SkNone,
    };
}

Stmnt stmnt_fndecl(FnDecl v, size_t index) {
    return (Stmnt){
        .kind = SkFnDecl,
        .cursors_idx = index,
        .fndecl = v,
    };
}

Stmnt stmnt_structdecl(StructDecl v, size_t index) {
    return (Stmnt){
        .kind = SkStructDecl,
        .cursors_idx = index,
        .structdecl = v,
    };
}

Stmnt stmnt_enumdecl(EnumDecl v, size_t index) {
    return (Stmnt){
        .kind = SkEnumDecl,
        .cursors_idx = index,
        .enumdecl = v,
    };
}

Stmnt stmnt_vardecl(VarDecl v, size_t index) {
    return (Stmnt){
        .kind = SkVarDecl,
        .cursors_idx = index,
        .vardecl = v,
    };
}

Stmnt stmnt_varreassign(VarReassign v, size_t index) {
    return (Stmnt){
        .kind = SkVarReassign,
        .cursors_idx = index,
        .varreassign = v,
    };
}

Stmnt stmnt_constdecl(ConstDecl v, size_t index) {
    return (Stmnt){
        .kind = SkConstDecl,
        .cursors_idx = index,
        .constdecl = v,
    };
}

Stmnt stmnt_return(Return v, size_t index) {
    return (Stmnt){
        .kind = SkReturn,
        .cursors_idx = index,
        .returnf = v,
    };
}

Stmnt stmnt_defer(Stmnt *v, size_t index) {
    return (Stmnt){
        .kind = SkDefer,
        .defer = v,
        .cursors_idx = index,
    };
}

Stmnt stmnt_continue(size_t index) {
    return (Stmnt){
        .kind = SkContinue,
        .cursors_idx = index,
    };
}

Stmnt stmnt_break(size_t index) {
    return (Stmnt){
        .kind = SkBreak,
        .cursors_idx = index,
    };
}

Stmnt stmnt_fall(size_t index) {
    return (Stmnt){
        .kind = SkFall,
        .cursors_idx = index,
    };
}

Stmnt stmnt_fncall(FnCall v, size_t index) {
    return (Stmnt){
        .kind = SkFnCall,
        .cursors_idx = index,
        .fncall = v,
    };
}

Stmnt stmnt_if(If v, size_t index) {
    return (Stmnt){
        .kind = SkIf,
        .cursors_idx = index,
        .iff = v,
    };
}

Stmnt stmnt_switch(Switch v, size_t index) {
    return (Stmnt) {
        .kind = SkSwitch,
        .cursors_idx = index,
        .switchf = v,
    };
}

Stmnt stmnt_case(Case v, size_t index) {
    return (Stmnt){
        .kind = SkCase,
        .cursors_idx = index,
        .casef = v,
    };
}

Stmnt stmnt_for(For v, size_t index) {
    return (Stmnt){
        .kind = SkFor,
        .cursors_idx = index,
        .forf = v,
    };
}

Stmnt stmnt_foreach(ForEach v, size_t index) {
    return (Stmnt){
        .kind = SkForEach,
        .cursors_idx = index,
        .foreach = v,
    };
}

Stmnt stmnt_block(Arr(Stmnt) v, size_t index) {
    return (Stmnt){
        .kind = SkBlock,
        .cursors_idx = index,
        .block = v,
    };
}

Stmnt stmnt_extern(Stmnt *v, size_t index) {
    return (Stmnt){
        .kind = SkExtern,
        .cursors_idx = index,
        .externf = v,
    };
}

Stmnt stmnt_directive(Directive v, size_t index) {
    return (Stmnt){
        .kind = SkDirective,
        .cursors_idx = index,
        .directive = v,
    };
}
