open Types

let matrix_dim = 101
let first (a,b) = a
let second (a,b) = b

(* ########################################################################################## *)

let print_model matrx = 
  for i = 0 to (matrix_dim-1) do 
   for j = 0 to (matrix_dim-1) do
          if (first matrx.(i).(j)) > 0 && (second matrx.(i).(j)) > 0 then print_string ("X ")  
     else if (second matrx.(i).(j)) > 0 then print_string ("~ ")
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

let point_occupied (p,s) = (p > 0) || (s > 0) 

let point_inside (x,y) rad =  
         let xf = float_of_int x
      in let yf = float_of_int y
      in (xf -. 50.)**2.0 +. (yf -. 50.)**2.0 < rad**2.0            


let count_occupied_nodes model = 
   let n = ref 0 in
   for i = 0 to (matrix_dim-1) do
    for j = 0 to (matrix_dim-1) do
          if (point_occupied model.(i).(j)) then n:= !n + 1
    done done;
    !n

let count_nodes_within rad model = 
      let n = ref 0 in
      for i = 0 to (matrix_dim-1) do
       for j = 0 to (matrix_dim-1) do
             if ((point_occupied model.(i).(j)) && (point_inside (i,j) rad)) 
             then n:= !n + 1
       done done;
       !n

let find_n_percent_occupied_radius model percent =       
      let number_of_occupied = float_of_int (count_occupied_nodes model) 
   in let rec count_down top = 
     if ((float_of_int (count_nodes_within top model)) /. number_of_occupied) > (percent /. 100.)
     then count_down (top -. 1.)
     else top +. 1.0 
    in  count_down 71. 


               

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)

let _ =
  let ic:in_channel = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel ic in
  let event_list = Parser.reactions Lexer.token lexbuf in
  let model = Array.make_matrix matrix_dim matrix_dim (0,0) in     
  let _ = get_end_state model event_list in 
  let k = find_n_percent_occupied_radius model 95. in 
       (* print_model model; *)
        print_string ((string_of_int (int_of_float k)) ^ "\n"); ()
        (* 
        print_string ( "" ^(string_of_int (count_occupied_nodes model)) ^"\n");
        print_string ( "" ^(string_of_int (count_nodes_within k model)) ^"\n") 
        *)
          
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~` *)
