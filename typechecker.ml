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
type fenv = (typ list * typ list) Env.t
type senv = (ident * typ) list Env.t

let dummy = "_"

let add_env l tenv =
  List.fold_left (fun env (x, t) -> if x = dummy then env else Env.add x t env) tenv l

let prog (fmt,ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let (fenv,senv) =
    List.fold_left
      (fun (fenv,senv) d ->
         match d with Struct(s) -> (fenv, Env.add s.sname.id s.fields senv)
                    | Fun(f)   -> let params_ty = List.map snd f.params in 
                                  let return_ty = f.return in
                                  (Env.add f.fname.id (params_ty, return_ty) fenv, senv))
      (Env.empty, Env.empty) ld
  in
  let check_typ t = match t with
    | TStruct s ->
      if Env.mem s senv then ()
      else error (Lexing.dummy_pos, Lexing.dummy_pos) (Printf.sprintf "Unknown structure %s" s)
    | _ -> ()
  in
  let check_fields lf = 
    let rec aux seen = function
      | [] -> ()
      | (id, t) :: tl ->
          if List.mem id.id seen then 
             error id.loc (Printf.sprintf "Duplicate field %s" id.id);
          check_typ t;
          aux (id.id :: seen) tl
    in aux [] lf
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
    | Binop (op, e1, e2) -> (match op with
        | Eq | Neq ->
            (match (e1.edesc, e2.edesc) with
            | (Nil, Nil) -> error e.eloc "Comparaison entre Nil et Nil"
            | (Nil, _) -> let t2 = type_expr e2 tenv in (match t2 with
              | TStruct _ -> TBool
              | _ -> error e.eloc (Printf.sprintf "Comparaison entre Nil et %s" (typ_to_string t2)))
            | (_, Nil) -> let t1 = type_expr e1 tenv in (match t1 with
              | TStruct _ -> TBool
              | _ -> error e.eloc (Printf.sprintf "Comparaison entre Nil et %s" (typ_to_string t1)))
            | (_, _) ->
            let t1 = type_expr e1 tenv in
            let t2 = type_expr e2 tenv in 
              if t1 = t2 then TBool
              else (type_error e.eloc t2 t1))
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
          else type_error e.eloc t1 TBool)
    | Unop (op, e) -> (match op with
      | Opp -> let t = type_expr e tenv in 
      if t = TInt then TInt else type_error e.eloc t TInt
      | Not -> let t = type_expr e tenv in 
      if t = TBool then TBool else type_error e.eloc t TBool)
    | Var id ->
      (try Env.find id.id tenv
      with Not_found -> error id.loc (Printf.sprintf "La variable %s n'est pas déclarée" id.id))
    | Dot (e, id) ->
      (match e.edesc with
        | Nil -> error e.eloc "accés illégal à un champ d'un pointeur Nil"
        | _ ->
          let t_e = type_expr e tenv in match t_e with
          | TStruct s -> let il = Env.find s senv in 
             let rec aux identtyplist = match identtyplist with
            | [] -> error e.eloc "type not found in struct" 
            | (id2, ty_id2) :: tl -> if id.id = id2.id then ty_id2
                                     else aux tl
            in aux il   
          | _ -> error e.eloc 
            (Printf.sprintf "Struct pointer expected, got %s" (typ_to_string t_e)))
    | New s -> if Env.mem s senv then TStruct s else error e.eloc (Printf.sprintf "Struct %s doesn't exist" s)
    | Call (id, args) -> 
      let (params_ty, return_ty) = Env.find id.id fenv in
      let args_types = List.map (fun arg -> type_expr arg tenv) args in
      (try
        let l = List.combine params_ty args_types in
        let comp_or_err (typ1,typ2) =
          if typ1 = typ2 then () else type_error e.eloc typ2 typ1
        in 
        List.iter comp_or_err l;
        match return_ty with
        | [] -> error e.eloc (Printf.sprintf "function %s returns void" id.id)
        | [typret] -> typret
        | _ -> error e.eloc (Printf.sprintf "function %s has type arity higher than 1" id.id)
      with Invalid_argument _ ->
        error e.eloc (Printf.sprintf "wrong number of arguments for function %s" id.id))
    | Nil -> error e.eloc "nil must be used in a context"
    | Print el ->
        (match el with
        | [{ edesc = Call (id, args); eloc = l}] ->
            let (params_ty, _) = Env.find id.id fenv in
            let args_types = List.map (fun arg -> type_expr arg tenv) args in
            (try
               let lp = List.combine params_ty args_types in
               List.iter (fun (t1, t2) -> if t1 = t2 then () else type_error l t2 t1) lp
             with Invalid_argument _ ->
               error l (Printf.sprintf "wrong number of arguments for function %s" id.id))
        | _ -> List.iter (fun e -> ignore (type_expr e tenv)) el);
        TInt
  in

  let rec check_instr i ret tenv = match i.idesc with
  | Expr e -> (match e.edesc with
    | Call (id, args) -> let (params_ty, return_ty) = Env.find id.id fenv in
    let args_types = List.map (fun arg -> type_expr arg tenv) args in
    (try
      let l = List.combine params_ty args_types in
      let comp_or_err (typ1,typ2) =
        if typ1 = typ2 then () else type_error e.eloc typ2 typ1
      in 
      List.iter comp_or_err l
      with Invalid_argument _ ->
      error e.eloc (Printf.sprintf "wrong number of arguments for function %s" id.id))
    | _ -> let _ = type_expr e tenv in ())
  | If (cond, b1, b2) -> 
      check_expr cond TBool tenv;
      check_seq b1 ret tenv;
      check_seq b2 ret tenv
  | For (cond, b) ->
      check_expr cond TBool tenv;
      check_seq b ret tenv
  | Return (el) ->
    (match el with
      (* On distingue le renvoi d'un appel de fonction *)
      | [{ edesc = Call (id, args); eloc = l}] ->
        let (params_ty, return_ty) = Env.find id.id fenv in
        let args_types = List.map (fun arg -> type_expr arg tenv) args in
        (try
          let lp = List.combine params_ty args_types in
          let comp_or_err (typ1,typ2) =
            if typ1 = typ2 then () else type_error l typ2 typ1
          in 
          List.iter comp_or_err lp;
        with Invalid_argument _ ->
          error l (Printf.sprintf "wrong number of arguments for function %s" id.id));
        (try
          let pairs = List.combine return_ty ret in
            List.iter (fun (t1, t2) -> if t1 = t2 then () else type_error l t2 t1) pairs
        with Invalid_argument _ ->
          error l (Printf.sprintf "wrong number of return values for function %s" id.id))
      | _ -> try
          let pairs = List.combine el ret in
            List.iter (fun (e, expected_typ) -> check_expr e expected_typ tenv) pairs
        with Invalid_argument _ -> error i.iloc "Wrong number of return values")
      | Set (lval, rval) ->
          (* On check les lvalue *)
          let rec check_lvalue e = match e.edesc with
            | Var _ -> ()
            | Dot (eprime, _) -> check_lvalue eprime
            | _ -> error e.eloc "assigning an rvalue to a non-lvalue"
          in 
          List.iter check_lvalue lval;
          (* Calcul des types attendus *)
          let ltypes = List.map (fun e -> type_expr e tenv) lval in
          (match rval with
          | [{edesc = Call (id, args); eloc = l}] ->
            let (params_ty, rt) = Env.find id.id fenv in
            let args_types = List.map (fun arg -> type_expr arg tenv) args in
          begin try
            let lp = List.combine params_ty args_types in
            List.iter (fun (typ1, typ2) -> if typ1 = typ2 then () else type_error l typ2 typ1) lp
          with Invalid_argument _ ->
            error l (Printf.sprintf "wrong number of arguments for function %s" id.id)
          end;
          begin try
              let pairs = List.combine ltypes rt in
              List.iter (fun (t1, t2) -> if t1 = t2 then () else type_error l t2 t1) pairs
            with Invalid_argument _ ->
              error l (Printf.sprintf "wrong number of return values for function %s" id.id)
          end
          | _ -> let rtypes = List.map (fun e -> type_expr e tenv) rval in
            try
              let pairs = List.combine ltypes rtypes in
              List.iter (fun (t1, t2) -> if t1 = t2 then () else type_error i.iloc t2 t1) pairs
            with Invalid_argument _ ->
              error i.iloc "wrong number of rvalues")
        | Vars (il, typ_opt, b) ->
                (match typ_opt with 
          | Some t -> let new_tenv =
              let idxtlist = List.map (fun id -> (id.id, t)) il
              in add_env idxtlist tenv
            in check_seq b ret new_tenv
          | None -> match b with
            | {idesc=Set(_, rval); _} :: tl -> 
              (* On récupére la rval *)
              let rtypes = match rval with
                | [{edesc = Call (id, args); eloc = l}] ->
                let (params_ty, rt) = Env.find id.id fenv in
                    let args_types = List.map (fun arg -> type_expr arg tenv) args in
                    (try
                      let lp = List.combine params_ty args_types in
                      List.iter (fun (expected, actual) -> 
                        if expected = actual then () else type_error l actual expected
                      ) lp
                    with Invalid_argument _ ->
                      error l (Printf.sprintf "wrong number of arguments for function %s" id.id));
                    rt (* On renvoie les types pour rtypes *)
                | _ ->
                List.map (fun e -> type_expr e tenv) rval in
              (try
                let newVars = List.combine (List.map (fun i -> i.id) il) rtypes in
                let new_tenv = add_env newVars tenv in
                check_seq tl ret new_tenv 
              with Invalid_argument _ ->
                error i.iloc "number of variables does not match number of values")
            | [] -> error i.iloc "variable declaration must have an initialization"
            | _ -> error i.iloc "variable declaration must have an initialization") 
          | Inc e | Dec e ->
            let rec check_lvalue e = match e.edesc with
              | Var _ -> ()
              | Dot (eprime, _) -> check_lvalue eprime
              | _ -> error e.eloc "expression is not an lvalue"
            in 
            check_lvalue e;
            (* ignore (type_expr e tenv) -> on vérifie juste que e est bien typée, supposons int *)
            let t = type_expr e tenv in
            if t <> TInt then type_error e.eloc t TInt
          | Block b -> check_seq b ret tenv

  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in
  
  let check_function f = 
    let params_map = List.map (fun (id, t) -> (id.id, t)) f.params in
    let new_tenv = add_env params_map Env.empty in
    check_seq f.body f.return new_tenv; (* On vérifie le corps *)
    (* Fonction auxiliaire permettant de vérifier que la fonction termine toujours sur un 
        return dans toutes ses branches *)
    let rec terminates instr_list =
      match instr_list with
      | [] -> false (* Pas de return *)
      | i :: tl ->
          let i_terminates = match i.idesc with
            | Return _ -> true
            | Block b -> terminates b
            | If(_, b1, b2) -> (terminates b1) && (terminates b2)
            | Vars (_,_,b) -> terminates b
            (* Si on a un for, le retour n'est pas garanti ? *)
            | _ -> false
          in
          if i_terminates then true else terminates tl
        in
        if f.return <> [] && not (terminates f.body) then
          error f.fname.loc "A return statement is missing in function body"


  in Env.iter (fun _ lf -> check_fields lf) senv;
     List.iter (function Fun f -> check_function f | _ -> ()) ld
     (* On itére sur tout l'AST, et quand on tombe sur une fonction on appelle check_function, comme ca on
        conserve l'accés à fenv dans check_function et au corps de la fonction
        Sinon on a l'erreur suivante : 
        274 |      Env.iter (fun _ fd -> check_function fd) fenv 
                                                            ^^^^
        Error: This expression has type (typ list * typ list) Env.t
        but an expression was expected of type func_def Env.t
        Type typ list * typ list is not compatible with type func_def
     *)
     (* RESTE A FAIRE : régle import fmt <=> (ssi) utilisation de fmt.Print*)