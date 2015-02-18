#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include "mpc.h"

typedef enum { LVAL_NUM, LVAL_ERR, LVAL_SYM, LVAL_SEXPR } value_type;

typedef struct lval {
  value_type type;
  long num;
  char* err;
  char* sym;
  int count;
  struct lval** cell;
} lval;

lval* lval_num(long x) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

lval* lval_err(char* m) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_ERR;
  v->err = malloc(strlen(m) + 1);
  strcpy(v->err, m);
  return v;
}

lval* lval_sym(char* s) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

lval* lval_sexpr(void) {
  lval* v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lval_del(lval* v) {
  switch (v->type) {
  case LVAL_NUM: break;
  case LVAL_ERR: free(v->err); break;
  case LVAL_SYM: free(v->sym); break;
  case LVAL_SEXPR:
    for (int i=0; i< v->count;i++) {
      lval_del(v->cell[i]);
    }
    free(v->cell);
    break;
  }
  free(v);
}

lval eval_op(lval left, char* op, lval right) {
  if (left.type == LVAL_ERR) { return left; }
  if (right.type == LVAL_ERR) { return right; }

  if (strcmp(op, "+") == 0) { return lval_num(left.num + right.num); }
  if (strcmp(op, "-") == 0) { return lval_num(left.num - right.num); }
  if (strcmp(op, "*") == 0) { return lval_num(left.num * right.num); }
  if (strcmp(op, "/") == 0) {
    return right.num == 0
      ? lval_err(LERR_DIV_ZERO)
      : lval_num(left.num / right.num);
  }
  if (strcmp(op, "^") == 0) { return lval_num(pow(left.num, right.num)); }
  if (strcmp(op, "%") == 0) { return lval_num(left.num % right.num); }
  if (strcmp(op, "max") == 0) { return lval_num(fmax(left.num, right.num)); }
  if (strcmp(op, "min") == 0) { return lval_num(fmin(left.num, right.num)); }

  return lval_err(LERR_BAD_OP);
}

void lval_print(lval v) {
  switch (v.type) {
  case LVAL_NUM: printf("%li", v.num); break;
  case LVAL_ERR:
    if (v.err == LERR_DIV_ZERO) {
      printf("Error: Division by Zero!");
    }
    if (v.err == LERR_BAD_OP) {
      printf("Error: Invalid Operator!");
    }
    if (v.err == LERR_BAD_NUM) {
      printf("Error: Invalid Number!");
    }
    break;
  }
}

void lval_println(lval v) { lval_print(v); putchar('\n');}

lval eval_unary(char* op, lval value) {
  if (value.type == LVAL_ERR) { return value; }
  if (strcmp(op, "-") == 0) { return lval_num(-value.num); }
  else return value;
}

lval eval(mpc_ast_t* t) {

  if(strstr(t->tag, "number")) {
    errno = 0;
    long x = strtol(t->contents, NULL, 10);
    return errno !=ERANGE ? lval_num(x) : lval_err(LERR_BAD_NUM);
  }

  char* op = t->children[1]->contents;
  lval x = eval(t->children[2]);

  if (t->children_num <= 4) {
    x = eval_unary(op, x);
  } else {
    int i = 3;
    while (strstr(t->children[i]->tag, "expr")) {
      x = eval_op(x, op, eval(t->children[i]));
      i++;
    }
  }
  return x;
}

int main(int argc, char** argv) {
  mpc_parser_t* Number = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* Sexpr = mpc_new("symbol");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
    "                                                                  \
     number   : /-?[0-9]+/ ;                                           \
     symbol   : '+' | '-' | '*' | '/' | '^' | '%' | \"min\" | \"max\" ;\
     sexpr    : '(' <expr>* ')' ;                                      \
     expr     : <number> | <symbol> | <sexpr> ;                        \
     lispy    : /^/ <expr>* /$/ ;                                      \
    ",
            Number, Symbol, Sexpr, Expr, Lispy);

  puts("Lispy version 0.0.0.0.1");
  puts("Press Ctrl+C to Exit\n");

  while (1) {
    char* input = readline("lispy> ");
    add_history(input);

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      lval result = eval(r.output);
      lval_println(result);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }

  mpc_cleanup(5, Number, Symbol, Sexpr, Expr, Lispy);
  return 0;
}
