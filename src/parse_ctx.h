#pragma once
#include "ast_bindings.h"

typedef struct {
    void *scanner;
    const Ast *out_ast;
} parse_ctx_t;