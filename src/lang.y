%code requires {
    #include "parse_ctx.h"
}

%define api.pure full
%define parse.error verbose
%lex-param {void* scanner}
%parse-param {parse_ctx_t *ctx}

%{
    #include "parse_ctx.h"
    #define scanner ctx->scanner
    
    #include <stdio.h>

    void yyerror(parse_ctx_t *ctx, const char *err) {
        fprintf(stderr, "%d:%d %s\n", yyget_lineno(scanner), yyget_column(scanner), err);
    }
%}

%union {
    ast_program *prgm;
    ast_expr_list *expr_list;
    ast_expr *expr;
    ast_literal *lit;
    ast_ident *ident;
    ast_case_list *case_list;
    const char *str;
}

%token
    PLUS "+"
    MINUS "-"
    STAR "*"
    DIV "/"
    MOD "%"
    BANG "!"
    LAMBDA "\\"
    ARROW "->"
    ASSIGN ":="
    ELLIPSIS "..."
    LPAREN "("
    RPAREN ")"
    IF "if"
    IS "is"
    THEN "then"
    ELSE "else"
    LET "let"
    DO "do"
    COMMA ","
    SEMI ";"
    TILDE "~"
    USTAR
    UMINUS
    COMPOUND
    FNCALL
%token <lit> LITERAL
%token <ident> IDENT

%type program
%type <expr_list> decl_list arg_list ident_list
%type <expr> expr decl ident_expr
%type <case_list> case_list

%right COMPOUND
%right ELSE
%right ASSIGN
%right ARROW
%left PLUS MINUS
%left STAR DIV MOD
%precedence UMINUS
%left BANG LPAREN
%precedence USTAR

%%

program
    : decl_list                             { ctx->out_ast = NEW(ast_program, { .exprs = $1 }); }
    ;

decl_list
    : decl decl_list                        { $$ = NEW(ast_expr_list, { .head = $1, .tail = $2 }); }
    | decl                                  { $$ = NEW(ast_expr_list, { .head = $1 }); }
    ;

decl
    : ident_expr ":=" expr                  { $$ = NEW(ast_expr, { .ty = AST_EXPR_VAR_DECL, .name = $1, .val = $3 }); }
    | ident_expr "~" expr                   { $$ = NEW(ast_expr, { .ty = AST_EXPR_ENTYPE, .name = $1, .val = $3 }); }
    | LAMBDA "(" ident_list ")" ":=" expr   { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_DECL, .pure_decl = true, .name = NULL, .arg_names = $3, .val = $6 }); }
    | ident_expr "(" ident_list ")" ":=" expr
                                            { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_DECL, .pure_decl = true, .name = $1, .arg_names = $3, .val = $6 }); }
    | ident_expr "!" "(" ident_list ")" ":=" expr
                                            { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_DECL, .pure_decl = false, .name = $1, .arg_names = $4, .val = $7 }); }
    ;

ident_list
    : ident_expr "," ident_list             { $$ = NEW(ast_expr_list, { .head = $1, .tail = $3 }); }
    | ident_expr                            { $$ = NEW(ast_expr_list, { .head = $1 }); }
    ;

arg_list
    : expr "," arg_list                     { $$ = NEW(ast_expr_list, { .head = $1, .tail = $3 }); }
    | expr                                  { $$ = NEW(ast_expr_list, { .head = $1 }); }
    | "..."                                 { $$ = NEW(ast_expr_list, { .head = NEW(ast_expr, { .ty = AST_EXPR_CURRY }) }); }
    ;

ident_expr
    : IDENT                                 { $$ = NEW(ast_expr, { .ty = AST_EXPR_IDENT, .ident = $1 }); }
    ;

expr
    : "let" decl ";" expr                   { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $2, .op = AST_OP_COMPOUND, .right = $4 }); } %prec COMPOUND
    | "do" expr ";" expr                    { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $2, .op = AST_OP_COMPOUND, .right = $4 }); } %prec COMPOUND
    | expr "+" expr                         { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $1, .op = AST_OP_ADD, .right = $3 }); }
    | expr "-" expr                         { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $1, .op = AST_OP_SUB, .right = $3 }); }
    | expr "*" expr                         { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $1, .op = AST_OP_MUL, .right = $3 }); }
    | expr "/" expr                         { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $1, .op = AST_OP_DIV, .right = $3 }); }
    | expr "%" expr                         { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $1, .op = AST_OP_MOD, .right = $3 }); }
    | expr "->" expr                        { $$ = NEW(ast_expr, { .ty = AST_EXPR_BINOP, .left = $1, .op = AST_OP_PREPEND, .right = $3 }); }
    | "-" expr                              { $$ = NEW(ast_expr, { .ty = AST_EXPR_UNARY, .op = AST_OP_NEGATE, .inner = $2 }); } %prec UMINUS
    | "*" expr                              { $$ = NEW(ast_expr, { .ty = AST_EXPR_UNARY, .op = AST_OP_HEAD, .inner = $2 }); } %prec USTAR
    | expr "->" "..."                       { $$ = NEW(ast_expr, { .ty = AST_EXPR_UNARY, .op = AST_OP_TAIL, .inner = $1 }); }
    | LITERAL                               { $$ = NEW(ast_expr, { .ty = AST_EXPR_LITERAL, .literal = $1 }); }
    | ident_expr                            { $$ = $1; }
    | "(" expr ")"                          { $$ = $2; }
    | "if" expr "then" expr "else" expr     { $$ = NEW(ast_expr, { .ty = AST_EXPR_IF, .cond = $2, .then = $4, .els = $6 }); }
    | "if" expr "is" "(" case_list ")"      { $$ = NEW(ast_expr, { .ty = AST_EXPR_IF_CASE, .cond = $2, .cases = $5}); }
    | expr "(" arg_list ")"                 { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_CALL, .pure_call = true, .callee = $1, .args = $3 }); }
    | expr "!" "(" arg_list ")"             { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_CALL, .pure_call = false, .callee = $1, .args = $4 }); }
    /* adapted from decl below */
    | ident_expr ":=" expr                  { $$ = NEW(ast_expr, { .ty = AST_EXPR_VAR_DECL, .name = $1, .val = $3 }); }
    | expr "(" arg_list ")" ":=" expr       { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_DECL, .pure_decl = true, .name = $1, .arg_names = $3, .val = $6 }); }
    | expr "!" "(" arg_list ")" ":=" expr   { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_DECL, .pure_decl = false, .name = $1, .arg_names = $4, .val = $7 }); }
    | LAMBDA "(" ident_list ")" ":=" expr   { $$ = NEW(ast_expr, { .ty = AST_EXPR_FN_DECL, .pure_decl = true, .name = NULL, .arg_names = $3, .val = $6 }); }
    ;

case_list
    : LITERAL "then" expr "," case_list     { $$ = NEW(ast_case_list, { .match = $1, .val = $3, .tail = $5 }); }
    | LITERAL "then" expr                   { $$ = NEW(ast_case_list, { .match = $1, .val = $3 }); }
    | "else" expr                           { $$ = NEW(ast_case_list, { .match = NULL, .val = $2 }); }
    ;
