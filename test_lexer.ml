open Mgoparser (* Important : pour accéder aux définitions de tokens *)

(* Fonction pour afficher joliment un token *)
let string_of_token = function
  | EOF -> "EOF" | PACKAGE -> "PACKAGE" | IMPORT -> "IMPORT" | TYPE -> "TYPE"
  | STRUCT -> "STRUCT" | ELSE -> "ELSE" | FALSE -> "FALSE" | FOR -> "FOR"
  | FUNC -> "FUNC" | IF -> "IF" | NIL -> "NIL" | RETURN -> "RETURN"
  | TRUE -> "TRUE" | VAR -> "VAR" | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | INT i -> Printf.sprintf "INT(%Ld)" i
  | STRING s -> Printf.sprintf "STRING(\"%s\")" s
  | SEMI -> "SEMI" | LPAR -> "LPAR" | RPAR -> "RPAR" | BEGIN -> "BEGIN" | END -> "END"
  | STAR -> "STAR" | COMMA -> "COMMA" | DOT -> "DOT" | COLONEQ -> "COLONEQ"
  | EQ -> "EQ" | PLUS -> "PLUS" | MINUS -> "MINUS" | DIV -> "DIV" | MOD -> "MOD"
  | EQ_OP -> "EQ_OP" | NEQ -> "NEQ" | LEQ -> "LEQ" | GEQ -> "GEQ" | LT -> "LT"
  | GT -> "GT" | AND -> "AND" | OR -> "OR" | NOT -> "NOT" | INC -> "INC" | DEC -> "DEC"

(* Fonction principale du test *)
let run_lexer filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    (* Boucle qui appelle le lexer et affiche les tokens jusqu'à EOF *)
    let rec loop () =
      let token = Mgolexer.token lexbuf in
      print_endline (string_of_token token);
      if token <> EOF then
        loop ()
    in
    loop ();
    close_in ic
  with
  | Mgolexer.Error msg -> (* Erreur personnalisée du lexer *)
      Printf.eprintf "Lexer error: %s\n" msg;
      exit 1
  | Sys_error msg -> (* Erreur système, ex: fichier non trouvé *)
      Printf.eprintf "System error: %s\n" msg;
      exit 1

(* Point d'entrée du programme *)
let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <fichier.go>\n" Sys.argv.(0)
  else
    run_lexer Sys.argv.(1)

