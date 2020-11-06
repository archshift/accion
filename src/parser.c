#include <stdio.h>

#include "lang.tab.h"
#include "lex.yy.h"

int parse_stdin(Ast **ast) {
    int res;
    yyscan_t scanner;
    res = yylex_init(&scanner);
    if (res) {
        printf("lex error: %d\n", res);
        return res;
    }

    parse_ctx_t ctx = {
        .scanner = scanner,
    };
    res = yyparse(&ctx);
    yylex_destroy(scanner);

    if (ast)
        *ast = ctx.out_ast;
    return res;
}