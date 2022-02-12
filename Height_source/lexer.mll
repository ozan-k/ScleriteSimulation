(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}

let blank = [' ' '\009' '\012']
let newline = ('\010' | '\013' | "\013\010")
let int_literal = ('-')? ['0'-'9'] ['0'-'9' '_']*
let float_literal =
('-')? ['0'-'9'] ['0'-'9' '_']*
('.' ['0'-'9' '_']* )?
(['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
let letter = ['A'-'Z' '_' '~' 'a'-'z']
let digit = ['0'-'9']
let alphanum = digit | letter
let ident =  letter alphanum*   

rule token = parse
    blank                 { token lexbuf }     (* skip blanks *)
  | newline               { token lexbuf }
  | ident as lxm          { IDENT(lxm) }
  | int_literal as lxm    { INT(lxm) }
  | float_literal as lxm  { VAL(lxm) }
  | ","                   { COMMA }
  | ";"                   { SEMICOLON } 
  | eof                   { EOF }
  