open Mgoast

exception Error of Mgoast.location * string
let error loc s = raise (Error (loc,s))

let type_error loc ty_actual ty_expected =
  error loc (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)

(* 3 environnements pour stocker
     les variables avec leur type,
     les fonctions avec leur signature
     les structures avec leurs champs
*)
    
type tenv = typ Env.t
type fenv = (typ list) * (typ list) Env.t
type senv = (ident * typ) list

let dummy = "_"

let add_env l tenv =
  List.fold_left (fun env (x, t) -> if x = dummy then env else Env.add x t env) tenv l

let prog (fmt,ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let (fenv,senv) =
    List.fold_left
      (fun (fenv,senv) d ->
         match d with Struct(s) -> (fenv, Env.add s.sname.id s.fields senv)
                    | Fun(f)   -> failwith "à compléter")
      (Env.empty, Env.empty) ld
  in
  let check_typ t = failwith "case not implemented in check_typ"
  in
  let check_fields lf = failwith "case not implemented in check_fields"
  in
  let rec check_expr e typ tenv =
    if e.edesc = Nil then  failwith "case not implemented in check"
    else let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error e.eloc typ_e typ
  and type_expr e tenv = match e.edesc with
    | Int _  -> TInt
    | String _ -> TString
    | Bool _ -> TBool
    (* à refactoriser, c'est beaucoup trop long *)
    | Binop (op, e1, e2) -> match op with
        | Eq | Neq ->
            match (e1.edesc, e2.edesc) with
            | (Nil, Nil) -> error e.eloc "Comparaison entre Nil et Nil"
            | (Nil, _) -> let t2 = type_expr e2 tenv in match t2 with
              | TStruct _ -> TBool
              | _ -> error e.eloc (Printf.sprintf "Comparaison entre Nil et %s" (typ_to_string t2))
            | (_, Nil) -> let t1 = type_expr e1 tenv in match t1 with
              | TStruct _ -> TBool
              | _ -> error e.eloc (Printf.sprintf "Comparaison entre Nil et %s" (typ_to_string t1))
            | (_, _) ->
            let t1 = type_expr e1 tenv in
            let t2 = type_expr e2 tenv in 
              if t1 = t2 then TBool
              else (type_error e.eloc t2 t1) 
        | Lt | Le | Gt | Ge ->
          let t1 = type_expr e1 tenv in
          let t2 = type_expr e2 tenv in 
          if t1 = TInt then
            if t2 = TInt then TBool
            else type_error e.eloc t2 TInt
          else type_error e.eloc t1 TInt
        | Add | Sub | Mul | Div | Rem ->
          let t1 = type_expr e1 tenv in
          let t2 = type_expr e2 tenv in 
          if t1 = TInt then
            if t2 = TInt then TInt
            else type_error e.eloc t2 TInt
          else type_error e.eloc t1 TInt
        | And | Or ->
          let t1 = type_expr e1 tenv in
          let t2 = type_expr e2 tenv in 
          if t1 = TBool then
            if t2 = TBool then TBool
            else type_error e.eloc t2 TBool
          else type_error e.eloc t1 TBool
      | Unop (op, e) -> match op with
        | Opp -> let t = type_expr e tenv in 
        if t = TInt then TInt else type_error e.eloc t TInt
        | Not -> let t = type_expr e tenv in 
        if t = TBool then TBool else type_error e.eloc t TBool
      | Var id ->
        try Env.find id.id tenv
        with Not_found -> error id.loc (Printf.sprintf "La variable %s n'est pas déclarée" id.id)
      (* !!!!!!! Il reste Dot Nil New Call Print !!!!!!!!*)
  in

  let rec check_instr i ret tenv = match i.idesc with
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in
  
  let check_function f = failwith "case not implemented in check_function"

  in Env.iter (fun _ lf -> check_fields lf) senv;
     Env.iter (fun _ fd -> check_function fd) fenv


