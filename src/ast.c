#include "ast.h"

#include <stdio.h>

void print_ident(ast_ident *i) {
    printf("(ident %s)", i->name);
}

void print_literal(ast_literal *l) {
    printf("(literal");
    switch (l->ty) {
        case AST_LITERAL_INT:
            printf(" %llu", l->num);
            break;
        case AST_LITERAL_STRING:
            printf(" %s", l->str);
            break;
    }
    printf(")");
}

void print_expr_list(ast_expr_list *l) {
    printf("(expr_list");
    for (; l; l = l->tail) {
        printf(" ");
        print_expr(l->head);
    }
    printf(")");
}

void print_case_list(ast_case_list *l) {
    printf("(case_list");
    for (; l; l = l->tail) {
        printf(" (case ");
        if (l->match) {
            print_literal(l->match);
        } else {
            printf("else");
        }
        printf(" ");
        print_expr(l->val);
        printf(")");
    }
    printf(")");
}

void print_expr(ast_expr *e) {
    printf("(expr");
    switch(e->ty) {
        case AST_EXPR_BINOP:
            printf(" binary");
            switch (e->op) {
                case AST_OP_ADD: printf(" add"); break;
                case AST_OP_SUB: printf(" sub"); break;
                case AST_OP_MUL: printf(" mul"); break;
                case AST_OP_DIV: printf(" div"); break;
                case AST_OP_MOD: printf(" mod"); break;
                case AST_OP_PREPEND: printf(" prepend"); break;
                case AST_OP_COMPOUND: printf(" compound"); break;
                default:
                    fprintf(stderr, "UNHANDLED binary operator type %d\n", e->op);
                    exit(-1);
            }
            printf(" ");
            print_expr(e->left);
            printf(" ");
            print_expr(e->right);
            break;
        case AST_EXPR_UNARY:
            printf(" unary");
            switch (e->op) {
                case AST_OP_HEAD: printf(" head"); break;
                case AST_OP_TAIL: printf(" tail"); break;
                case AST_OP_NEGATE: printf(" negate"); break;
                default:
                    fprintf(stderr, "UNHANDLED unary operator type %d\n", e->op);
                    exit(-1);
            }
            printf(" ");
            print_expr(e->inner);
            break;
        case AST_EXPR_IDENT:
            printf(" ident ");
            print_ident(e->ident);
            break;
        case AST_EXPR_LITERAL:
            printf(" literal ");
            print_literal(e->literal);
            break;
        case AST_EXPR_VAR_DECL:
            printf(" decl ");
            print_expr(e->name);
            printf(" ");
            print_expr(e->val);
            break;
        case AST_EXPR_ENTYPE:
            printf(" entype ");
            print_expr(e->name);
            printf(" ");
            print_expr(e->val);
            break;
        case AST_EXPR_FN_DECL:
            printf(" fn_decl ");
            if (e->pure_decl)
                printf("pure ");
            else
                printf("impure ");
            if (e->name) {
                print_expr(e->name);
                printf(" ");
            } else
                printf("lambda ");
            print_expr_list(e->arg_names);
            printf(" ");
            print_expr(e->val);
            break;
        case AST_EXPR_IF:
            printf(" if ");
            print_expr(e->cond);
            printf(" ");
            print_expr(e->then);
            printf(" ");
            print_expr(e->els);
            break;
        case AST_EXPR_IF_CASE:
            printf(" if_case ");
            print_expr(e->cond);
            printf(" ");
            print_case_list(e->cases);
            break;
        case AST_EXPR_FN_CALL:
            printf(" call ");
            if (e->pure_call)
                printf("pure ");
            else
                printf("impure ");
            print_expr(e->callee);
            printf(" ");
            print_expr_list(e->args);
            break;
        case AST_EXPR_CURRY:
            printf(" curry");
            break;
        default:
            fprintf(stderr, "UNHANDLED expression type %d\n", e->ty);
            exit(-1);
    }
    printf(")");
}

void print_program(ast_program *p) {
    printf("(program");
    for (ast_expr_list *l = p->exprs; l; l = l->tail) {
        printf(" ");
        print_expr(l->head);
    }
    printf(")");
}