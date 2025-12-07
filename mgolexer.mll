{
  open Lexing
  open Mgoparser
  let semi = ref false
  exception Error of string

  (* Cette expression nous renvoie une fonction qui permet de retrouver les mots clés lors de l'extraction de nos lexèmes, 
     et renvoie le token correspondant, ou sinon le transforme en ident *)
  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k) (* On ajoute les mots clés du langage ici *)
    [ "package",    PACKAGE;
      "import",     IMPORT;
      "type",       TYPE;      
      "struct",     STRUCT;
      "else",       ELSE;
      "false",      FALSE;
      "for",        FOR;
      "func",       FUNC;
      "if",         IF;
      "nil",        NIL;
      "return",     RETURN;
      "true",       TRUE;
      "var",        VAR;      
    ] ;
  fun s ->
    try  
      let token = Hashtbl.find h s in
      match token with
      | FALSE | NIL | RETURN | TRUE -> semi := true; token
      | _ -> semi := false; token
    with Not_found -> 
    semi := true;
    IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let hexa = digit | ['a'-'f' 'A'-'F'] (* Caractère hexadécimal *)

(* regex pour identificateur et numéro *)
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let fmt = "fmt" 

(* regex pour types atomiques *)
let integer = number | ("0x" | "0X" ) hexa+ (* entier (en base 10 ou en base 16) *)
let char = [' ''!''#'-'[' ']'-'~' '\n''\t'] | "\\\\"  | "\\\"" | "\\n" | "\\t" (* caractère *)
let str = '"' char* '"'

rule token = parse
  | ['\n']            { new_line lexbuf; if !semi then 
                          (semi := false; SEMI)
                        else token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "/*"              { comment lexbuf; token lexbuf }

  | "//" [^'\n']*  { token lexbuf }  (* commentaire sur une ligne, on ne capture pas le \n pour pouvoir rentrer dans la première régle, pour la gestion
                                        des point virgule automatiques à l'avenir *)

  | '"' fmt '"'       { semi := true; STRING("fmt") }

  | ("0x" | "0X") hexa+ as n  { semi := true; try INT(Int64.of_string n) 
                             with _ -> raise (Error "literal constant too large") }
  | number as n  { semi := true; try INT(Int64.of_string n) 
                   with _ -> raise (Error "literal constant too large") }
  | ident as id  { keyword_or_ident id }

  | str as s { semi := true; STRING(String.sub s 1 (String.length s - 2)) }  (* On enlève les guillemets *)

  | ";"  { semi := false; SEMI }
  | "("  { semi := false; LPAR }
  | ")"  { semi := true; RPAR }
  | "{"  { semi := false; BEGIN }
  | "}"  { semi := true; END }
  | "*"  { semi := false; STAR } (* Sera utilisé pour la multiplication ET la gestion de pointeurs, on peut pas le mettre ds opérateurs arithmétiques*)
  | ","  { semi := false; COMMA }
  | "."  { semi := false; DOT }

  (* affectation *)
  | ":=" { semi := false; COLONEQ }
  | "="  { semi := false; EQ }

  (* opérateurs arithmétiques *)
  | "+"  { semi := false; PLUS }
  | "-"  { semi := false; MINUS }
  | "/"  { semi := false; DIV }
  | "%"  { semi := false; MOD }

  (* opérateurs de comparaison *)
  | "==" { semi := false; EQ_OP }
  | "!=" { semi := false; NEQ }
  | "<=" { semi := false; LEQ }
  | ">=" { semi := false; GEQ }
  | "<"  { semi := false; LT }
  | ">"  { semi := false; GT }

  (* opérateurs logiques *)
  | "&&" { semi := false; AND }
  | "||" { semi := false; OR }
  | "!"  { semi := false; NOT }

  (* incrémentation/décrémentation *)
  | "++" { semi := true; INC }
  | "--" { semi := true; DEC }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { if !semi then 
            (semi := false; SEMI)
           else EOF }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
