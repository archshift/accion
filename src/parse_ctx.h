#pragma once
#include "ast.h"

typedef struct {
    void *scanner;
    ast_program *out_ast;
} parse_ctx_t;