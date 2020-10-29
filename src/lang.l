%{
    #include "lang.tab.h"

    #include <string.h>
%}

%option reentrant
%option bison-bridge
%option noyywrap
%option yylineno

%%

"if"                                    { return IF; }
"is"                                    { return IS; }
"then"                                  { return THEN; }
"else"                                  { return ELSE; }
"let"                                   { return LET; }
"do"                                    { return DO; }

[a-zA-Z_][0-9a-zA-Z_]*                  {   yylval->ident = NEW(ast_ident, {
                                                .name = strdup(yytext)
                                            });
                                            return IDENT; }
[1-9][0-9]*                             {   yylval->lit = NEW(ast_literal, {
                                                .ty = AST_LITERAL_INT,
                                                .num = strtoull(yytext, NULL, 10),
                                            });
                                            if (errno == ERANGE) {
                                                return YYerror;
                                            } else {
                                                return LITERAL;
                                            }
                                        }

\"(\\.|[^"])*\"                         {   char *str = strdup(yytext + 1);
                                            str[yyleng - 2] = 0;
                                            yylval->lit = NEW(ast_literal, {
                                                .ty = AST_LITERAL_STRING,
                                                .str = str
                                            });
                                            return LITERAL; }

"+"                                     { return PLUS; }
"-"                                     { return MINUS; }
"*"                                     { return STAR; }
"/"                                     { return DIV; }
"%"                                     { return MOD; }
"~"                                     { return TILDE; }
"!"                                     { return BANG; }
"\\"                                    { return LAMBDA; }
"->"                                    { return ARROW; }
":="                                    { return ASSIGN; }
"("                                     { return LPAREN; }
")"                                     { return RPAREN; }
"..."                                   { return ELLIPSIS; }
","                                     { return COMMA; }
";"                                     { return SEMI; }

[ \n\t]                                 {}
