#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include "mpc.h"


long eval_op(long left, char* op, long right) {
  if (strcmp(op, "+") == 0) { return left + right; }
  if (strcmp(op, "-") == 0) { return left - right; }
  if (strcmp(op, "*") == 0) { return left * right; }
  if (strcmp(op, "/") == 0) { return left / right; }
  if (strcmp(op, "^") == 0) { return pow(left, right); }
  if (strcmp(op, "%") == 0) { return left % right; }
  if (strcmp(op, "max") == 0) { return fmax(left, right); }
  if (strcmp(op, "min") == 0) { return fmin(left, right); }
  return 0;
}

long eval_unary(char* op, long value) {
  if (strcmp(op, "-") == 0) { return -value; }
  else return value;
}

long eval(mpc_ast_t* t) {

  if(strstr(t->tag, "number")) {
    return atoi(t->contents);
  }

  char* op = t->children[1]->contents;
  long x = eval(t->children[2]);

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
  mpc_parser_t* Operator = mpc_new("operator");
  mpc_parser_t* Expr = mpc_new("expr");
  mpc_parser_t* Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
    "                                                                  \
     number   : /-?[0-9]+/ ;                                           \
     operator : '+' | '-' | '*' | '/' | '^' | '%' | \"min\" | \"max\" ;\
     expr     : <number> | '(' <operator> <expr>+ ')' ;                \
     lispy    : /^/ <operator> <expr>+ /$/ ;                           \
    ",
            Number, Operator, Expr, Lispy);

  puts("Lispy version 0.0.0.0.1");
  puts("Press Ctrl+C to Exit\n");

  while (1) {
    char* input = readline("lispy> ");
    add_history(input);

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      long result = eval(r.output);
      printf("%li\n", result);
      mpc_ast_delete(r.output);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }

    free(input);
  }

  mpc_cleanup(4, Number, Operator, Expr, Lispy);
  return 0;
}
