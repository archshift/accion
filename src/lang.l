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

[a-zA-Z_][0-9a-zA-Z_]*                  {   yylval->str = strdup(yytext);
                                            return IDENT; }
[0-9]+                                  {   yylval->num = strtoull(yytext, NULL, 10);
                                            if (errno == ERANGE) {
                                                return YYerror;
                                            } else {
                                                return LITERAL_INT;
                                            }
                                        }

\"(\\.|[^"])*\"                         {   char *str = strdup(yytext + 1);
                                            str[yyleng - 2] = 0;
                                            yylval->str = str;
                                            return LITERAL_STR; }

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
"=="                                    { return EQ; }

#[^\n]*\n                               {}
[ \n\t]                                 {}
