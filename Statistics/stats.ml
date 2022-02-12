open Types

(* ########################################################################################## *)

type proc = (string * ( float * float ) * float )

(* float * int * (int * int) * (int * int) *)
let rec quantify_results event_list k = 
   match event_list with [] -> k |
     (_time,event,_,_)::tail -> if event = 2
                                then quantify_results tail (k+1)
                                else quantify_results tail k

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)

let _ =
  let ic:in_channel = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel ic in
  let event_list = Parser.reactions Lexer.token lexbuf in
       let polyp_count = quantify_results event_list 0 in
       print_string ("\n" ^ (string_of_int  polyp_count) ^ "\n")

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
