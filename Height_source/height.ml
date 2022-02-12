open Types

let matrix_dim = 101
let first (a,b) = a
let second (a,b) = b

(* ########################################################################################## *)

let print_model matrx = 
  for i = 0 to (matrix_dim-1) do 
   for j = 0 to (matrix_dim-1) do
          if (first matrx.(i).(j)) > 0 && (second matrx.(i).(j)) > 0 then print_string ("X" ^ (string_of_int (second matrx.(i).(j))))  
     else if (second matrx.(i).(j)) > 0 then print_string (" " ^ (string_of_int (second matrx.(i).(j))) )
     else print_string ("_ ")
   done;
   print_string "\n";
   done 

(* ########################################################################################## *)

let increase_one_budding (x,y) = (x+1,y)

let decrease_one_sclerite (x,y) = (x,y-1)

let increase_one_sclerite (x,y) = (x,y+1)


(* float * int * (int * int) * (int * int) *)
let rec get_end_state model event_list =  match event_list with 
                                               [] -> ()  
  |   (_time,1,(x_from,y_from),(x_to,y_to))::tail -> model.(x_from).(y_from) <- decrease_one_sclerite model.(x_from).(y_from);
                                                     model.(x_to).(y_to) <- increase_one_sclerite model.(x_to).(y_to);
                                                     get_end_state model tail
  | (_time,2,(_x_from,_y_from),(x_to,y_to))::tail -> model.(x_to).(y_to) <- increase_one_budding model.(x_to).(y_to);
                                                     get_end_state model tail
  | (_time,3,(_x_from,_y_from),(x_to,y_to))::tail -> model.(x_to).(y_to) <- increase_one_sclerite model.(x_to).(y_to);
                                                     get_end_state model tail                                                        
  | _ -> print_string "."

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)

let find_the_heighest_point model =       
      let max = ref 0 
       in let _ = for i = 0 to (matrix_dim-1) do
           for j = 0 to (matrix_dim-1) do 
            if  (second model.(i).(j)) > !max 
            then max := (second model.(i).(j))
            else ()
            done done 
   in !max 

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)

let find_the_approximate_height model =       
  let sum1 = (second model.(50).(50)) + (second model.(50).(45)) + (second model.(45).(50)) in
  let sum2 = sum1 + (second model.(50).(55)) + (second model.(55).(50)) in
   int_of_float ((float_of_int sum2) /. 5.) 


let _ =
let ic:in_channel = open_in (Sys.argv.(1)) in
let lexbuf = Lexing.from_channel ic in
let event_list = Parser.reactions Lexer.token lexbuf in
let model = Array.make_matrix matrix_dim matrix_dim (0,0) in     
let _ = get_end_state model event_list in 
(* let _ = print_model model in *)
(* let k = find_the_heighest_point model in  *)
let k = find_the_approximate_height model in
     print_string ((string_of_int k) ^ "\n"); ()
       (* find_n_percent_occupied_radius model 95. in  *)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)

