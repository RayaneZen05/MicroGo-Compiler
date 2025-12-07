open Mgoast
open Mips
module Env = Map.Make(String) (* On va utiliser des maps pour gérer les décalages de pile *)
let struct_sizes  = Hashtbl.create 10
let field_offsets = Hashtbl.create 20
let func_returns  = Hashtbl.create 20 (* C'est juste pour pouvoir print des n-uplets, coûte une passe compléte du programme*)
(* On remplie les tables *)
let collect_structs declarations =
  List.iter (function
  | Struct s -> 
      let size = 4 * List.length s.fields in
      Hashtbl.add struct_sizes s.sname.id size;
      
      List.iteri (fun i (id, _) -> Hashtbl.add field_offsets id.id (i*4)) s.fields
  | _ -> ()
    ) declarations
let collect_funcs declarations =
  List.iter (function
    | Fun f -> 
        Hashtbl.add func_returns f.fname.id (List.length f.return)
    | _ -> ()
  ) declarations
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
    lw t0 ofs fp
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
  | New s -> let size = try Hashtbl.find struct_sizes s 
                 with Not_found -> failwith ("Unknown struct " ^ s) in
      li a0 size    (* appel système pour alloc mémoire*)
      @@ li v0 9      
      @@ syscall     
      @@ move t0 v0  
  | Print (el) ->
    let code_print = List.fold_left (fun acc e ->
      acc 
      @@ tr_expr e venv
      @@ (match e.edesc with
      | String _ -> move a0 t0 @@ li v0 4 @@ syscall
      | Call (id, _) -> let nb_ret = try Hashtbl.find func_returns id.id with Not_found -> 1 in
        if nb_ret > 1 then
          move t1 t0
          @@ (
            let rec print_tuple i =
              if i >= nb_ret then nop
              else
                lw a0 (i*4) t1
                @@ li v0 1
                @@ syscall
                @@ print_tuple (i+1)
              in print_tuple 0
          )
            else
              move a0 t0 @@ li v0 1 @@ syscall
        | _ -> move a0 t0 @@ li v0 1 @@ syscall) 
    ) nop el in
    code_print @@ li t0 0
    | Dot (e, id) -> 
      let ofs = try Hashtbl.find field_offsets id.id
    with Not_found -> failwith ("Unknown field" ^ id.id) in
    tr_expr e venv
    @@ lw t0 ofs t0


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
  | Return (el) -> 
      let n = List.length el in
      let calcule_ret =
        match el with 
      | []  -> li t0 0
      | [e] -> tr_expr e venv (* on charge la valeur de retour dans t0 *)
      | _ -> 
          li a0 (n * 4) (* Appel sys pour allouer le bloc qui contiendra les valeurs *)
          @@ li v0 9
          @@ syscall
          @@ move t1 v0 
          @@ fst (List.fold_left (fun (code, idx) e ->
            let c = 
              code
              @@ push t1 (* on évite d'écraser l'@ du bloc *)
              @@ tr_expr e venv
              @@ pop t1
              @@ sw t0 (idx * 4) t1 (* On écrit t0 dans bloc[idx] *)
            in (c, idx+1)) (nop, 0) el) @@ move t0 t1   
      (* épilogue *)
      in calcule_ret
      @@ addi sp fp (-4)
      @@ pop ra
      @@ pop fp
      @@ jr ra
  | Set (lvals, [{edesc = Call(f, args); _} as call]) when List.length lvals > 1 ->
      (* on a l'adresse du nuplet dans t0*)
      let code_call = tr_expr call venv in
      let code_save = push t0 in
      let code_assign =
        fst (List.fold_left (fun (code, i) lval -> 
          let load_val = 
            lw t1 0 sp
            @@ lw t0 (i*4) t1
         in
         let assign = match lval.edesc with
           | Var(id) ->
               if Env.mem id.id venv then
                 let ofs = Env.find id.id venv in sw t0 ofs fp
               else
                 la t1 id.id @@ sw t0 0 t1
           | Dot(e, id) -> 
               let ofs = Hashtbl.find field_offsets id.id in
               push t0
               @@ tr_expr e venv
               @@ pop t1
               @@ sw t1 ofs t0
           | _ -> failwith "Assigning to non-lvalue"
           in 
          (code, i+1)) (nop, 0) lvals)
        in code_call @@ code_save @@ code_assign @@ addi sp sp 4
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
          | Dot(e, id) ->
            let ofs = try Hashtbl.find field_offsets id.id
          with Not_found -> failwith ("Unknown field" ^ id.id) in
            tr_expr e venv
            @@ push t0
            @@ tr_expr rv venv
            @@ pop t1
            @@ sw t0 ofs t1
            @@ aux t
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
       | Dot (e, id) -> let ofs = try Hashtbl.find field_offsets id.id
        with Not_found -> failwith ("Unknown field" ^ id.id) in
        tr_expr e venv
        @@ lw t1 ofs t0
        @@ addi t1 t1 1
        @@ sw t1 ofs t0
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
      | Dot (e, id) ->
        let ofs = try Hashtbl.find field_offsets id.id
        with Not_found -> failwith ("Unknown field" ^ id.id) in
        tr_expr e venv
        @@ lw t1 ofs t0
        @@ addi t1 t1 (-1)
        @@ sw t1 ofs t0
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
  collect_structs p;
  collect_funcs p;
  let code_text = tr_ldecl p in (* On s'assure du bon ordre d'évaluation *)
  let code_entry = jal "main" (* on saute à main *)
  @@ li v0 10 (* appel system de sortie *)
  @@ syscall 
in 
  { text = code_entry @@ code_text ; data = !data_segment }
