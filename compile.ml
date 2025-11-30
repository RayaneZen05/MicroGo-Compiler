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
  let label = new_str () in
  let d = label lb @@ asciiz s in
  data_segment := !data_segment @@ d
  la t0 lbl
  
(* le résultat de l'expression est dans le registre $t0,
   la pile est utilisée pour les valeurs intermédiaires *)
let rec tr_expr e venv = match e.edesc with
  | Int(n)  -> li t0 (Int64.to_int n)   (* on supposera que les constantes entières
                                           sont représentables sur 32 bits *)
  | String(s) -> w_str s (* charge l'adresse du string dans t0 *)
  | Var(id) ->
    try 
    let ofs = Env.find id.id venv in
    lw t0 ofs sp
    with Not_found -> failwith (Printf.sprintf "undefined variable %s" id.id) (* Envisager de mieux gérer les erreurs ensuite *)
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
  | Call (id, args) -> failwith "prochaine étape ..."


let rec tr_seq = function
  | []   -> nop
  | [i]  -> tr_instr i
  | i::s -> tr_instr i @@ tr_seq s

and tr_instr i = match i.idesc with 
  | If(c, s1, s2) ->
    let then_label = new_label()
    and end_label = new_label()
    in
    tr_expr c venv
    @@ bnez t0 then_label
    @@ tr_seq s2
    @@ b end_label
    @@ label then_label
    @@ tr_seq s1
    @@ label end_label

  | For(c, s) ->
    let test_label = new_label()
    and code_label = new_label()
    in
    b test_label
    @@ label code_label
    @@ tr_seq s
    @@ label test_label
    @@ tr_expr c venv
    @@ bnez t0 code_label
  | _ -> failwith "A compléter"

let tr_fun df =
       label df.fname.id
    @@ tr_seq df.body

let rec tr_ldecl = function
    Fun df::p -> tr_fun df @@ tr_ldecl p
  | _ :: p -> tr_ldecl p
  | [] -> nop
  
let tr_prog p =  { text = tr_ldecl p ; data = !data_segment }
