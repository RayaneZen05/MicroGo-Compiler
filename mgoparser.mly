%{
  exception Error
%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING
%token PACKAGE IMPORT TYPE STRUCT ELSE FALSE FOR FUNC IF
%token NIL RETURN TRUE VAR
%token SEMI LPAR RPAR BEGIN END STAR COMMA DOT
%token COLONEQ EQ PLUS MINUS DIV MOD
%token EQ_OP NEQ LEQ GEQ LT GT
%token AND OR NOT INC DEC
%token EOF

%start prog
%type <Mgoast.program> prog

%%

prog:
  | EOF { (false, []) }
  | PACKAGE IDENT SEMI EOF { (false, []) }
  | PACKAGE IDENT SEMI IMPORT STRING SEMI EOF { (true, []) }
;