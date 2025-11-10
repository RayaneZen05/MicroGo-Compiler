%{

  open Lexing
  open Mgoast

  exception Error

%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING
%token PACKAGE IMPORT TYPE STRUCT ELSE FALSE FOR FUNC IF
%token NIL RETURN TRUE VAR
%token SEMI LPAR RPAR BEGIN END COMMA
%token COLONEQ 
%token EOF

(* types des opérateurs *)
%left OR 
%left AND
%left EQ NEQ GT GEQ LT LEQ
%left PLUS MINUS
%left STAR DIV MOD
%nonassoc UMINUS NOT INC DEC
%left DOT

%start prog
%type <Mgoast.program> prog

%%

prog:
| PACKAGE main=IDENT SEMI decls=list(decl) EOF
    { if main="main" then (false, decls) else raise Error}
| PACKAGE main=IDENT SEMI IMPORT fmt=STRING SEMI decls=list(decl) EOF
    { if main="main" && fmt="fmt" then (true, decls) else raise Error} 
;

decl:
 TYPE id=ident STRUCT BEGIN fl=loption(fields) (* peut contenir une liste de champs ou rien *) END SEMI (* déclaration de type *)
  { Struct { sname = id; fields = List.flatten fl; } }
  (* return type = mgotype OU varstyp ? *)
 | FUNC id=ident LPAR vst=varstyps RPAR rt = ret_type (* loption(separated_list(COMMA, varstyp)) *)
  BEGIN il=loption(instr) END SEMI (* déclaration de fonction *)
  { Fun { fname = id; params = vst; return=rt; body = il} }  
;

ident:
  id = IDENT { { loc = $startpos, $endpos; id = id } }
;


mgotype:
  | INT { TInt}
  | STRING { TString }
  | FALSE | TRUE { TBool }
  | NIL { TStruct("") } (* nil est un pointeur nul vers une structure inconnue ? *)
  | STAR s=IDENT { TStruct(s) }
;
ret_type:
  | t=mgotype { t }
  | (lt=list(mgotype) ,?) { lt }
;
varstyp:
  |  x=ident t=mgotype               {[(x,t)]}
  ;
(* !!!!VERIFIER!!!! *)
varstyps:
  | x=ident t=mgotype COMMA varstyps  { (x,t) :: varstyps } (* bizzare dans le sujet : ident,+ mgotype *)
  | x,t=varstyp                 { [(x,t)] }
;

fields:
| xt=varstyp SEMI?              { [xt]      }
| xt=varstyp SEMI xtl = fields  { xt :: xtl }
;
op: (* VERIFIER SI YA TOUT *)
| EQ_OP {Binop(Eq)} | NEQ { Binop(Neq) } | LEQ { Binop(Le)} | GEQ { Binop(Ge) } | LT {Binop(Lt)} | GT {Binop(Gt)} (*comp*)
| PLUS {Binop(Add)} | MINUS { Binop(Min)}| STAR { Binop(Mul)} | DIV {Binop(Div)}| MOD {Binop(Rem)} 
| AND {Binop(And)}  | OR {Binop(Or)}     


expr:
| e = expr_desc {  { eloc = $startpos, $endpos; edesc = e } }
;
expr_desc:
| n=INT { Int(n) }
| s=STRING { String(s) }
| t=TRUE { Bool(true) }
| f=FALSE { Bool(false) }
| ( e=expr_desc ) { e }
| id = ident { Var(id) }
| e=expr DOT id=ident { Dot (e, id) }
| id=ident ( el = loption(expr)) { Call (id, el) }
| (* IL FAUT GERER FMT.PRINT *)
| NOT e=expr { Unop(Not, e) }
| UMINUS e=expr { Unop(Opp, e) }
| e1=expr oper=op e2=expr { Binop(op, e1, e2) }
;
