#include <stdio.h>

#include "lang.tab.h"
#include "lex.yy.h"

enum yyin_type {
    YYIN_TYPE_STDIN,
    YYIN_TYPE_FILE,
    YYIN_TYPE_MEM,
};

int set_yyin_file(yyscan_t scanner, const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) return -1;
    yyset_in(f, scanner);
    return 0;
}

int set_yyin_buf(yyscan_t scanner, const uint8_t *mem, size_t len) {
    FILE *f = fmemopen((char*)mem, len, "r");
    if (!f) return -1;
    yyset_in(f, scanner);
    return 0;
}

// If ytype == YYIN_TYPE_STDIN, mem and limit are ignored.
// If ytype == YYIN_TYPE_FILE, mem should point to the filename, and limit is ignored.
// If ytype == YYIN_TYPE_MEM, mem should point to the buffer, and limit is the buffer length.
int parse_yyin(const Ast **ast, enum yyin_type ytype, const char *mem, size_t limit) {
    int res;
    yyscan_t scanner;
    res = yylex_init(&scanner);
    if (res) {
        printf("lex error: %d\n", res);
        return res;
    }

    switch (ytype) {
        case YYIN_TYPE_STDIN: break;
        case YYIN_TYPE_MEM:
            res = set_yyin_buf(scanner, (const uint8_t *)mem, limit);
            break;
        case YYIN_TYPE_FILE:
            res = set_yyin_file(scanner, mem);
            break;
    }
    if (res) {
        printf("error %d: error opening input `%s`\n", res, mem);
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