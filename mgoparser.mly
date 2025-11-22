%{
  open Mgoast

  (* Fonction auxiliaire pour créer un noeud instruction avec sa position *)
  let mk_instr desc start_p end_p = 
    { idesc = desc; iloc = (start_p, end_p) }

  (* Fonction auxiliaire pour créer un noeud expression avec sa position *)
  let mk_expr desc start_p end_p =
    { edesc = desc; eloc = (start_p, end_p) }

  (* Vérifie qu'une liste d'expressions ne contient que des variables (pour :=) *)
  let verif_ids exprs =
    List.map (fun e -> 
      match e.edesc with 
      | Var(id) -> id 
      | _ -> raise Parsing.Parse_error
    ) exprs
%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING
%token PACKAGE IMPORT TYPE STRUCT ELSE FALSE FOR FUNC IF
%token NIL RETURN TRUE VAR
%token SEMI LPAR RPAR BEGIN END COMMA
%token COLONEQ EQ
%token PLUS MINUS STAR DIV MOD
%token EQ_OP NEQ GT GEQ LT LEQ
%token AND OR NOT INC DEC DOT
%token EOF

%nonassoc ELSE        
%left OR              
%left AND             
%left EQ_OP NEQ LT LEQ GT GEQ 
%left PLUS MINUS      
%left STAR DIV MOD    
%nonassoc UMINUS NOT
%left DOT             

%start prog
%type <Mgoast.program> prog

%%


prog:
| PACKAGE main=IDENT SEMI fmt=option(import_fmt) decls=list(decl) EOF
    { if main <> "main" then raise Parsing.Parse_error;
      let has_fmt = match fmt with Some _ -> true | None -> false in
      (has_fmt, decls) }
;

import_fmt:
| IMPORT fmt=STRING SEMI { if fmt = "fmt" then true else raise Parsing.Parse_error }
;

decl:
| TYPE id=ident STRUCT BEGIN fl=loption(fields) END SEMI
    { Struct { sname = id; fields = List.flatten fl; } }
| FUNC id=ident LPAR vst=varstyps RPAR rt=ret_type il=bloc SEMI
    { Fun { fname = id; params = List.flatten vst; return = rt; body = il} }
;

ident:
| id = IDENT { { loc = $startpos, $endpos; id = id } }
;

mgotype:
| id=IDENT 
    { match id with 
      | "int" -> TInt | "bool" -> TBool | "string" -> TString 
      | _ -> raise Parsing.Parse_error }
| STAR id=IDENT { TStruct(id) }
;

ret_type:
| /* vide */ { [] }
| t=mgotype { [t] }
| LPAR tl=separated_list(COMMA, mgotype) RPAR { tl }
;

varstyp:
| ids=separated_nonempty_list(COMMA, ident) t=mgotype { List.map (fun i -> (i,t)) ids }
;

varstyps:
| l=separated_list(COMMA, varstyp) { l }
;

fields:
| xt=varstyp SEMI? { [xt] }
| xt=varstyp SEMI xtl=fields { xt :: xtl }
;

expr:
| e = expr_desc { mk_expr e $startpos $endpos }
;

expr_desc:
| n=INT { Int(n) }
| s=STRING { String(s) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| NIL { Nil }
| id=ident { Var(id) }
| LPAR e=expr_desc RPAR { e }
| NOT e=expr { Unop(Not, e) }
| MINUS e=expr %prec UMINUS { Unop(Opp, e) }
/* choix volontaire de les mettre inline au lieu de créer une régle op
 afin de pouvoir gérer les priorités, voir pourquoi la factorisation en op ne marchait pas */
| e1=expr PLUS e2=expr { Binop(Add, e1, e2) }
| e1=expr MINUS e2=expr { Binop(Sub, e1, e2) }
| e1=expr STAR e2=expr { Binop(Mul, e1, e2) }
| e1=expr DIV e2=expr { Binop(Div, e1, e2) }
| e1=expr MOD e2=expr { Binop(Rem, e1, e2) }
| e1=expr EQ_OP e2=expr { Binop(Eq, e1, e2) }
| e1=expr NEQ e2=expr { Binop(Neq, e1, e2) }
| e1=expr LT e2=expr { Binop(Lt, e1, e2) }
| e1=expr LEQ e2=expr { Binop(Le, e1, e2) }
| e1=expr GT e2=expr { Binop(Gt, e1, e2) }
| e1=expr GEQ e2=expr { Binop(Ge, e1, e2) }
| e1=expr AND e2=expr { Binop(And, e1, e2) }
| e1=expr OR e2=expr { Binop(Or, e1, e2) }
| e=expr DOT id=ident { Dot (e, id) }
| id=ident LPAR el=separated_list(COMMA, expr) RPAR 
    { 
      if id.id = "new" then
        match el with
        | [{edesc=Var(s); _}] -> New(s.id) 
        | _ -> raise Parsing.Parse_error
      else Call (id, el) 
    }
/* Gestion unifiée de fmt.Print pour éviter les conflits */
| e=expr DOT print=IDENT LPAR el=separated_list(COMMA, expr) RPAR 
    { 
      (* On vérifie ici sémantiquement que c'est bien fmt.Print *)
      match e.edesc, print with
      | Var({id="fmt";_}), "Print" -> Print(el)
      | _ -> raise Parsing.Parse_error 
    }
;

bloc:
| BEGIN il=instrs END { il } 
;

instrs:
| /* eps/cas de base */ { [] }
| i=instr { i [] }            /* Dernière instruction sans point-virgule */
| i=instr SEMI l=instrs { i l } /* Instruction avec point-virgule */
;

instr:
| i=instr_simple { i }
| b=bloc { fun s -> (mk_instr (Block b) $startpos $endpos) :: s }
| i=instr_if { i }
| RETURN el=separated_list(COMMA, expr) 
    { fun s -> (mk_instr (Return el) $startpos $endpos) :: s }
| VAR ids=separated_nonempty_list(COMMA, ident) t=option(mgotype) init=option(preceded(EQ, separated_nonempty_list(COMMA, expr)))
    { fun s -> 
        let es = match init with None -> [] | Some e -> e in
        let set_instr = match es with 
            | [] -> s 
            | _ -> (mk_instr (Set(List.map (fun i -> mk_expr (Var i) (fst i.loc) (snd i.loc)) ids, es)) $startpos $endpos) :: s 
        in
        [ mk_instr (Vars(ids, t, set_instr)) $startpos $endpos ]
    }
| FOR c=expr b=bloc 
    { fun s -> (mk_instr (For(c, b)) $startpos $endpos) :: s }
| FOR b=bloc 
    { fun s -> (mk_instr (For(mk_expr (Bool true) $startpos $endpos, b)) $startpos $endpos) :: s }
| FOR init=option(instr_simple) SEMI c=expr SEMI step=option(instr_simple) b=bloc
    { fun s ->
       (* Transformation de la boucle for complète en while (cf. page 5 sujet) *)
       let body_with_step = match step with 
            | None -> b 
            | Some st -> b @ (st []) (* On applique step à une suite vide pour l'avoir en fin de bloc *)
       in
       let loop_instr = mk_instr (For(c, body_with_step)) $startpos $endpos in
       let block_content = match init with 
            | None -> [loop_instr] 
            | Some i -> i [loop_instr] (* On applique init à la suite contenant la boucle *)
       in
       (mk_instr (Block(block_content)) $startpos $endpos) :: s
    }
;

instr_simple:
/* Unification de (expr), (expr)++, (expr)--, (assign), (decl courte :=) */
| el=separated_nonempty_list(COMMA, expr) suffix=simple_suffix
    { suffix el $startpos $endpos }
;

simple_suffix:
    { fun el start_p end_p s -> 
      match el with
      | [e] -> (mk_instr (Expr e) start_p end_p) :: s
      | _ -> raise Parsing.Parse_error (* "expr, expr" n'est pas une instruction valide sans = ou := *)
    }
| INC 
    { fun el start_p end_p s ->
      match el with
      | [e] -> (mk_instr (Inc e) start_p end_p) :: s
      | _ -> raise Parsing.Parse_error 
    }
| DEC 
    { fun el start_p end_p s ->
      match el with
      | [e] -> (mk_instr (Dec e) start_p end_p) :: s
      | _ -> raise Parsing.Parse_error 
    }
| EQ er=separated_nonempty_list(COMMA, expr)
    { fun el start_p end_p s -> 
      (mk_instr (Set(el, er)) start_p end_p) :: s 
    }
| COLONEQ er=separated_nonempty_list(COMMA, expr)
    { fun el start_p end_p s ->
      (* VERIFIER!!!!! la gauche de := doit être des identifiants *)
      let ids = verif_ids el in
      let set_instr = mk_instr (Set(el, er)) start_p end_p in
      [ mk_instr (Vars(ids, None, set_instr :: s)) start_p end_p ]
    }
;

instr_if:
| IF e=expr b=bloc { fun s -> (mk_instr (If(e, b, [])) $startpos $endpos) :: s }
| IF e=expr b1=bloc ELSE b2=bloc { fun s -> (mk_instr (If(e, b1, b2)) $startpos $endpos) :: s }
| IF e=expr b1=bloc ELSE i2=instr_if 
    { fun s -> 
      (* i2 est une fonction seq->seq, on l'applique à [] pour récupérer l'instruction If imbriquée *)
      let nested_if = i2 [] in 
      (mk_instr (If(e, b1, nested_if)) $startpos $endpos) :: s 
    }
;