%code requires {
    #include "parse_ctx.h"
}

%define api.pure full
%locations
%define parse.error verbose
%lex-param {void* scanner}
%parse-param {parse_ctx_t *ctx}

%{
    #include "lang.tab.h"
    #define scanner ctx->scanner
    
    #include <stdio.h>

    void yyerror(YYLTYPE *loc, parse_ctx_t *ctx, const char *err) {
        (void)ctx;
        fprintf(stderr, "%d:%d %s\n", loc->first_line, loc->first_column, err);
    }

    #define LOC (*(Location *)&yylloc)
%}

%union {
    const Ast *prgm;
    const Ident *ident;
    Expr *expr;
    Literal *lit;
    ExprLLNode *expr_list;
    CaseLL *case_list;

    char *str;
    uint64_t num;
    bool boolean;
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
    EQ "=="
    NIL "nil"
    USTAR
    UMINUS
    COMPOUND
%token <num> LITERAL_INT
%token <str> LITERAL_STR
%token <boolean> LITERAL_BOOL
%token <str> IDENT

%type program
%type <lit> literal
%type <expr_list> decl_list arg_list ident_list
%type <expr> expr decl ident_expr
%type <case_list> case_list

%right COMPOUND
%right ELSE
%right ASSIGN
%nonassoc EQ
%right ARROW
%left PLUS MINUS
%left STAR DIV MOD
%precedence UMINUS
%left BANG LPAREN
%precedence USTAR

%%

program
    : decl_list                             { ctx->out_ast = ast_create($1); }
    ;

decl_list
    : decl decl_list                        { $$ = ast_prepend_expr_list($1, $2); }
    | decl                                  { $$ = ast_prepend_expr_list($1, NULL); }
    ;

decl
    : ident_expr ":=" expr                  { $$ = ast_add_expr_var_decl(LOC, $1, $3); }
    | ident_expr "~" expr                   { $$ = ast_add_expr_entype(LOC, $1, $3); }
    | LAMBDA "(" ident_list ")" ":=" expr   { $$ = ast_add_expr_fn_decl(LOC, true, NULL, $3, $6); }
    | ident_expr "(" ident_list ")" ":=" expr
                                            { $$ = ast_add_expr_fn_decl(LOC, true, $1, $3, $6); }
    | ident_expr "!" "(" ident_list ")" ":=" expr
                                            { $$ = ast_add_expr_fn_decl(LOC, false, $1, $4, $7); }
    ;

ident_list
    : ident_expr "," ident_list             { $$ = ast_prepend_expr_list($1, $3); }
    | ident_expr                            { $$ = ast_prepend_expr_list($1, NULL); }
    ;

arg_list
    : expr "," arg_list                     { $$ = ast_prepend_expr_list($1, $3); }
    | expr                                  { $$ = ast_prepend_expr_list($1, NULL); }
    | "..."                                 { $$ = ast_prepend_expr_list(ast_add_expr_curry(LOC), NULL); }
    ;

ident_expr
    : IDENT                                 { $$ = ast_add_expr_ident(LOC, ast_add_ident(LOC, $1) ); }
    ;

expr
    : "let" decl ";" expr                   { $$ = ast_add_expr_binary(LOC, $2, BinaryOp_LastUnit, $4); } %prec COMPOUND
    | "do" expr ";" expr                    { $$ = ast_add_expr_binary(LOC, $2, BinaryOp_LastUnit, $4); } %prec COMPOUND
    | expr "+" expr                         { $$ = ast_add_expr_binary(LOC, $1, BinaryOp_Add, $3); }
    | expr "-" expr                         { $$ = ast_add_expr_binary(LOC, $1, BinaryOp_Sub, $3); }
    | expr "*" expr                         { $$ = ast_add_expr_binary(LOC, $1, BinaryOp_Mul, $3); }
    | expr "/" expr                         { $$ = ast_add_expr_binary(LOC, $1, BinaryOp_Div, $3); }
    | expr "%" expr                         { $$ = ast_add_expr_binary(LOC, $1, BinaryOp_Mod, $3); }
    | expr "->" expr                        { $$ = ast_add_expr_binary(LOC, $1, BinaryOp_Prepend, $3); }
    | expr "==" expr                        { $$ = ast_add_expr_binary(LOC, $1, BinaryOp_Eq, $3); }
    | "-" expr                              { $$ = ast_add_expr_unary(LOC, UnaryOp_Negate, $2); } %prec UMINUS
    | "*" expr                              { $$ = ast_add_expr_unary(LOC, UnaryOp_Head, $2); } %prec USTAR
    | expr "->" "..."                       { $$ = ast_add_expr_unary(LOC, UnaryOp_Tail, $1); }
    | literal                               { $$ = ast_add_expr_literal(LOC, $1); }
    | ident_expr                            { $$ = $1; }
    | "(" expr ")"                          { $$ = $2; }
    | "if" expr "then" expr "else" expr     { $$ = ast_add_expr_if(LOC, $2, $4, $6); }
    | "if" expr "is" "(" case_list ")"      { $$ = ast_add_expr_if_case(LOC, $2, $5); }
    | expr "(" arg_list ")"                 { $$ = ast_add_expr_fn_call(LOC, true, $1, $3 ); }
    | expr "!" "(" arg_list ")"             { $$ = ast_add_expr_fn_call(LOC, false, $1, $4 ); }
    /* adapted from decl below */
    | ident_expr ":=" expr                  { $$ = ast_add_expr_var_decl(LOC, $1, $3); }
    | expr "(" arg_list ")" ":=" expr       { $$ = ast_add_expr_fn_decl(LOC, true, $1, $3, $6); }
    | expr "!" "(" arg_list ")" ":=" expr   { $$ = ast_add_expr_fn_decl(LOC, false, $1, $4, $7); }
    | LAMBDA "(" ident_list ")" ":=" expr   { $$ = ast_add_expr_fn_decl(LOC, true, NULL, $3, $6); }
    ;

case_list
    : literal "then" expr "," case_list     { $$ = ast_prepend_case_list($1, $3, $5); }
    | literal "then" expr                   { $$ = ast_prepend_case_list($1, $3, NULL); }
    | "else" expr                           { $$ = ast_prepend_case_list(NULL, $2, NULL); }
    ;

literal
    : LITERAL_INT                           { $$ = ast_add_literal_int(LOC, $1); }
    | LITERAL_STR                           { $$ = ast_add_literal_str(LOC, $1); }
    | LITERAL_BOOL                          { $$ = ast_add_literal_bool(LOC, $1); }
    | NIL                                   { $$ = ast_add_literal_nil(LOC); }
    ;
