{
  open Lexing
  open Mgoparser

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
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
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
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "/*"              { comment lexbuf; token lexbuf }

  | "//" [^'\n']*  { token lexbuf }  (* commentaire sur une ligne, on ne capture pas le \n pour pouvoir rentrer dans la première régle, pour la gestion
                                        des point virgule automatiques à l'avenir *)

  | '"' fmt '"'       { STRING("fmt") }

  | ("0x" | "0X") hexa+ as n  { try INT(Int64.of_string n) 
                             with _ -> raise (Error "literal constant too large") }
  | number as n  { try INT(Int64.of_string n) 
                   with _ -> raise (Error "literal constant too large") }
  | ident as id  { keyword_or_ident id }

  | str as s { STRING(String.sub s 1 (String.length s - 2)) }  (* On enlève les guillemets *)

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "*"  { STAR } (* Sera utilisé pour la multiplication ET la gestion de pointeurs, on peut pas le mettre ds opérateurs arithmétiques*)
  | ","  { COMMA }
  | "."  { DOT }

  (* affectation *)
  | ":=" { COLONEQ }
  | "="  { EQ }

  (* opérateurs arithmétiques *)
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "/"  { DIV }
  | "%"  { MOD }

  (* opérateurs de comparaison *)
  | "==" { EQ_OP }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<"  { LT }
  | ">"  { GT }

  (* opérateurs logiques *)
  | "&&" { AND }
  | "||" { OR }
  | "!"  { NOT }

  (* incrémentation/décrémentation *)
  | "++" { INC }
  | "--" { DEC }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
