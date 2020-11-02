#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

struct ast_program_s;
struct ast_expr_s;
struct ast_expr_list_s;
struct ast_literal_s;
struct ast_ident_s;
struct ast_case_list_s;

#define NEW(s, ...) ({ \
    s *_ptr = (s *)malloc(sizeof(s)); \
    s _rval = __VA_ARGS__; \
    *_ptr = _rval; \
    _ptr; })

typedef struct ast_program_s {
    struct ast_expr_list_s *exprs;
} ast_program;

enum ast_expr_type {
    AST_EXPR_BINOP,
    AST_EXPR_UNARY,
    AST_EXPR_VAR_DECL,
    AST_EXPR_FN_DECL,
    AST_EXPR_FN_CALL,
    AST_EXPR_LITERAL,
    AST_EXPR_IDENT,
    AST_EXPR_IF,
    AST_EXPR_IF_CASE,
    AST_EXPR_CURRY,
    AST_EXPR_ENTYPE,
};

enum ast_operator {
    AST_OP_MUL,
    AST_OP_DIV,
    AST_OP_ADD,
    AST_OP_SUB,
    AST_OP_MOD,
    AST_OP_NEGATE,
    AST_OP_PREPEND,
    AST_OP_HEAD,
    AST_OP_TAIL,
    AST_OP_COMPOUND,
    AST_OP_EQ,
};

typedef struct ast_expr_s {
    enum ast_expr_type ty;
    union {
        struct ast_literal_s *literal;
        struct ast_ident_s *ident;
        // Unary/binary operations
        struct {
            enum ast_operator op;
            union {
                // Binary operands
                struct {
                    struct ast_expr_s *left, *right;
                };
                // Unary operand
                struct ast_expr_s *inner;
            };
        };
        // Variable/function decls
        struct {
            struct ast_expr_s *name;
            struct ast_expr_s *val;
            union {
                struct {};
                struct {
                    struct ast_expr_list_s *arg_names;
                    bool pure_decl;
                };
            };
        };
        // If statement
        struct {
            struct ast_expr_s *cond;
            union {
                // Normal
                struct {
                    struct ast_expr_s *then, *els;
                };
                // Cases
                struct ast_case_list_s *cases;
            };
        };
        // Function call
        struct {
            struct ast_expr_s *callee;
            struct ast_expr_list_s *args;
            bool pure_call;
        };
    };
} ast_expr;

typedef struct ast_expr_list_s {
    struct ast_expr_s *head;
    struct ast_expr_list_s *tail;
} ast_expr_list;

enum ast_literal_type {
    AST_LITERAL_INT,
    AST_LITERAL_STRING,
};

typedef struct ast_literal_s {
    enum ast_literal_type ty;
    union {
        char *str;
        uint64_t num;
    };
} ast_literal;

typedef struct ast_ident_s {
    char *name;
} ast_ident;

typedef struct ast_case_list_s {
    struct ast_literal_s *match;
    struct ast_expr_s *val;
    struct ast_case_list_s *tail;
} ast_case_list;

void print_expr(ast_expr *);
void print_program(ast_program *);