open Mgoast
open Mips
module Env = Map.Make(String) (* On va utiliser des maps pour gérer les décalages de pile *)
let data_segment = ref Mips.nop (* On initialise le segment .data vide *)
let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "_label_%i" !cpt
(* Labels des chaînes de caractéres dans le segment data *)
let new_str =
  let cptstr = ref (-1) in
  fun () -> incr cptstr; Printf.sprintf "_string_%i" !cptstr
(* fonction d'écriture dans le segment data *)
let w_str s =
  let lbl = new_str () in
  let d = label lbl @@ asciiz s in
  data_segment := !data_segment @@ d;
  la t0 lbl
  
(* le résultat de l'expression est dans le registre $t0,
   la pile est utilisée pour les valeurs intermédiaires *)
let rec tr_expr e venv = match e.edesc with
  | Int(n)  -> li t0 (Int64.to_int n)   (* on supposera que les constantes entières
                                           sont représentables sur 32 bits *)
  | String(s) -> w_str s (* charge l'adresse du string dans t0 *)
  | Var(id) ->
    begin try 
    let ofs = Env.find id.id venv in
    lw t0 ofs sp
    with Not_found -> failwith (Printf.sprintf "undefined variable %s" id.id) (* Envisager de mieux gérer les erreurs ensuite *)
    end;
    | Binop(bop, e1, e2) ->
    let op = match bop with
      | Add -> add
      | Mul -> mul
      | Lt  -> slt
      | Le  -> (fun r1 r2 r3 -> 
                slt r1 r3 r2
                @@ xori r1 r1 1)
      | Gt  -> (fun r1 r2 r3 -> slt r1 r3 r2)
      | Ge  -> (fun r1 r2 r3 ->
                slt r1 r2 r3 (* on calcule r2 < r3 *)
                @@ xori r1 r1 1 (* On fait le xor avec 1 : c'est comme faire un not *))
      | Eq  -> (fun r1 r2 r3 ->
                xor r1 r2 r3 
                @@ sltiu r1 r1 1 (* On check si 0 <= r2 - r3 < 1 : nécéssairement, r2 - r3 = 0 d'ou r2 = r3*)) 
      | Neq -> (fun r1 r2 r3 ->
                xor r1 r2 r3 
                @@ sltiu r1 r1 1
                @@ xori r1 r1 1) 
      | And -> and_
      | Or  -> or_
      | Sub -> sub
      | Div -> div
      | Rem -> rem
    in
    tr_expr e2 venv
    @@ push t0
    @@ tr_expr e1 venv
    @@ pop t1
    @@ op t0 t0 t1
  | Call (id, args) -> 
      (* On empile les arguments *)
      let call_args = List.fold_left (fun code arg -> code @@ tr_expr arg venv @@ push t0) nop args in
      (* On saute vers le label de la fct, et on sauvegarde l'adresse de retour dans ra *)
      let code_call = jal id.id in (*on fait quoi de la return address ?*)
      (* nettoyage de la pile *)
      let code_clean = addi sp sp (4 * (List.length args)) in
      call_args @@ code_call @@ code_clean
  | New s -> failwith "à faire" 
  | Print (el) ->
    let code_print = List.fold_left (fun acc e ->
      acc 
      @@ tr_expr e venv
      @@ move a0 t0
      @@ (match e.edesc with
          | String _ -> li v0 4
          | _ -> li v0 1
          )
      @@ syscall
    ) nop el in
    code_print @@ li t0 0


let rec tr_seq s venv ofs = match s with
  | []   -> nop
  | [i]  -> tr_instr i venv ofs
  | i::s -> tr_instr i venv ofs @@ tr_seq s venv ofs

and tr_instr i venv ofs = match i.idesc with 
  | If(c, s1, s2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
    tr_expr c venv
    @@ bnez t0 then_label
    @@ tr_seq s2 venv ofs
    @@ b end_label
    @@ label then_label
    @@ tr_seq s1 venv ofs
    @@ label end_label

  | For(c, s) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    b test_label
    @@ label code_label
    @@ tr_seq s venv ofs
    @@ label test_label
    @@ tr_expr c venv
    @@ bnez t0 code_label
  | Vars(il, _, seq) ->
      let rec loc_vars il venv ofs = 
      match il with
      | [] -> tr_seq seq venv ofs
      | h :: t -> li t0 0 @@ push t0 @@ loc_vars t (Env.add h.id ofs venv) (ofs-4)
      in 
      loc_vars il venv ofs
      @@ addi sp sp (4 * List.length il)
  | Return (el) -> begin match el with (* TODO: Gérer valeurs multiples *)
      | [e] -> 
        tr_expr e venv (* on charge la valeur de retour dans t0 *)
        @@ addi sp fp (-4)
        @@ pop ra
        @@ pop fp
        @@ jr ra 
      | _ -> failwith "On verra ça aprés ?"
      end;
  | Set (lval, rval) ->
      begin try
        let rec aux lvars = 
        match lvars with
        | (lv, rv) :: t -> 
          begin match lv.edesc with 
          | Var(id) -> 
            if Env.mem id.id venv then
              let var_ofs = Env.find id.id venv in
              tr_expr rv venv
              @@ sw t0 var_ofs fp
              @@ aux t
            else (* else inutile : on a pas de var. glob en mgo, à supprimer *)
              (*la t1 id.id
              @@ sw t0 0 t1*)
              failwith "plus tard..."
          | _ -> failwith "wrong lvalue"
          end;
        | [] -> Mips.nop
        in aux (List.combine lval rval) 
      with Invalid_argument _ -> failwith "impossible"
      end;
    | Expr e -> tr_expr e venv
    | Inc e -> begin match e.edesc with
      | Var (id) ->
        begin try 
        let v = Env.find id.id venv in
        tr_expr e venv
        @@ addi t0 t0 1 
        @@ sw t0 v fp
      with Not_found -> failwith (Printf.sprintf "%s value not found" id.id)
      end;
       | _ -> failwith "trying to increment non-lvalue"
    end;
    | Dec e -> match e.edesc with
      | Var (id) ->
        begin try 
        let v = Env.find id.id venv in
        tr_expr e venv
        @@ addi t0 t0 (-1)
        @@ sw t0 v fp
      with Not_found -> failwith "Printf.sprintf '%s value not found' id.id" (*régler*)
    end;
      | _ -> failwith "trying to decrement non-lvalue"

let tr_fun df =
      (* On inverse la liste pour commencer par le dernier argument (celui en haut de la pile) *)
      let params_inv = List.rev df.params in
      (* L'accumulateur est une paire : (environnement, offset_actuel) *)
      let (venv_args, _) = List.fold_left 
        (fun (env, ofs) (arg_id, arg_typ) -> 
            let new_env = Env.add arg_id.id ofs env in (new_env, ofs+4)) 
        (Env.empty, 4) (* Pourquoi on commence à 4 si on a déja fait "addi fp sp 4 ?" *)
        params_inv in
    (* On prépare le tableau d'activation *)
    label df.fname.id
    @@ push fp
    @@ push ra
    @@ addi fp sp 4      
    @@ tr_seq df.body venv_args (-8)
    (* epilogue *)
    @@ addi sp fp (-4)
    @@ pop ra
    @@ pop fp
    @@ jr ra

let rec tr_ldecl = function
    Fun df::p -> tr_fun df @@ tr_ldecl p
  | _ :: p -> tr_ldecl p
  | [] -> nop
  
let tr_prog p =  
  let code_text = tr_ldecl p in (* On s'assure du bon ordre d'évaluation *)
  { text = code_text ; data = !data_segment }
