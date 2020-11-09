%{
    #include "lang.tab.h"

    #include <string.h>

    #define YY_USER_ACTION do { \
        yylloc->first_line = yylloc->last_line; \
        yylloc->first_column = yylloc->last_column; \
        char *txt, *prev = yytext; \
        while ((txt = strchr(prev, '\n'))) { \
            yylloc->last_line++; \
            yylloc->last_column = 0; \
            prev = txt + 1; \
        } \
        yylloc->last_column += strlen(prev); \
    } while (0);
%}

%option reentrant
%option bison-bridge
%option bison-locations
%option noyywrap
%option yylineno

%%

"if"                                    { return IF; }
"is"                                    { return IS; }
"then"                                  { return THEN; }
"else"                                  { return ELSE; }
"let"                                   { return LET; }
"do"                                    { return DO; }
"nil"                                   { return NIL; }

"true"                                  {   yylval->boolean = true;
                                            return LITERAL_BOOL; }
"false"                                 {   yylval->boolean = false;
                                            return LITERAL_BOOL; }

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
