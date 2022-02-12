open Bigarray

let matrix_dim = 101
let first (a,b) = a
let second (a,b) = b
let min a b = if a < b then a else b 

(* ######################################################### *)
(* ######################################################### *)

let print_model matrx = 
   for i = 0 to (matrix_dim-1) do 
    for j = 0 to (matrix_dim-1) do
           if (first matrx.(i).(j)) > 0 && (second matrx.(i).(j)) > 0 then print_string ("X ")  
      else if (second matrx.(i).(j)) > 0 then print_string ("~ ")
      else print_string ("_ ")
    done;
    print_string "\n";
    done  
(*
let print_float s = print_string ((string_of_float s) ^ "\n")    

let pd (a,b,c) = print_string ((string_of_int a) ^ "," ^ (string_of_int b) ^ "," ^ c ^ "\n") 
*)
let print_cell matrx x y = 
   let (c,l) = matrx.(x).(y) in
   print_string ( "cell (" ^ (string_of_int x) ^  "," ^ (string_of_int y)^") -> polyp: " ^ (string_of_int c) ^  " , sclerite: " ^ (string_of_int l) ^ "\n")

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* ######################################################### *)
(* ######################################################### *)

let in_boundaries x_new y_new =  x_new > 0  &&  y_new > 0  &&  x_new < (matrix_dim-1)  &&  y_new < (matrix_dim-1) 

let next_action aj =
  let u = (Random.float aj.(0)) in
  let rec next countR n = if countR >= u
                      then n
            else next (countR +. aj.(n+1)) (n+1) in
     next 0. 0

let compute_tau a0 =  ( 1. /. a0 ) *. ( log ( 1.0 /. (Random.float 1.0 ) ) )

(* ######################################################### *)
(* ######################################################### *)
(* ~~~~~~~~~~~~~~~~~~ General funtions ~~~~~~~~~~~~~~~~~~~~~ *) 

let rec add_to_cells_with_check lst cells = match lst with [] -> cells | 
           h::tail -> if List.mem h cells
                      then add_to_cells_with_check tail cells
                      else add_to_cells_with_check tail (h::cells)

let remove_from_list (x,y) this_lst =
  let rec lur lst aux = match lst with [] -> aux |
     (a,b,c)::tail -> if a = x && b = y
                    then aux @ tail
                    else lur tail ((a,b,c)::aux)
    in lur this_lst []

(* ------------------------------------------------------------- *)

let set_model_diffusion_after_event matrx_model matrx_diffusion  (x,y)  = 
   let height_xy = second  matrx_model.(x).(y) 
in let delta p q = height_xy - ( second  matrx_model.(p).(q) ) 
in let delta_main p q = if (in_boundaries p q) && (delta p  q ) > 0 
                        then [( p,q,delta p  q) ] 
                        else [] 
in let result = ( delta_main (x+1)  y   )
              @ ( delta_main   x  (y+1) ) 
              @ ( delta_main (x-1)  y   )
              @ ( delta_main   x  (y-1) ) 
in let semafor = (in_boundaries x y)
in if semafor then matrx_diffusion.(x).(y) <- result else ()

let rec set_model_diff_after_event_for_cell_list   matrx_model   matrx_diffusion   cell_lst = match cell_lst with [] -> () |
             h::tail -> (set_model_diffusion_after_event matrx_model matrx_diffusion h);
                        (set_model_diff_after_event_for_cell_list   matrx_model   matrx_diffusion tail)

(* ------------------------------------------------------------- *)

let empty_surronding x_new y_new matrx_model = 
  (first matrx_model.(x_new).(y_new+1)) = 0 && 
  (* (first matrx_model.(x_new+1).(y_new+1)) = 0 && *)
  (first matrx_model.(x_new+1).(y_new)) = 0 && 
  (* (first matrx_model.(x_new+1).(y_new-1)) = 0 && *)
  (first matrx_model.(x_new).(y_new-1)) = 0 && 
  (* (first matrx_model.(x_new-1).(y_new-1)) = 0 && *)
  (first matrx_model.(x_new-1).(y_new)) = 0 
  (* &&  *)
  (* (first matrx_model.(x_new-1).(y_new+1)) = 0 *)
   
let set_fertile_cells_after_event  matrx_model fertile_cells  (x_new,y_new) = 
  let height = second matrx_model.(x_new).(y_new) in 
  if first matrx_model.(x_new).(y_new) = 0  &&  second matrx_model.(x_new).(y_new) = 1 && (empty_surronding x_new y_new matrx_model)  
  then (x_new,y_new,height)::fertile_cells
  else fertile_cells               

let conditional_fertile_remove  matrix  (x,y)  fertile_list = 
   if second matrix.(x).(y) = 0
   then remove_from_list (x,y) fertile_list
   else fertile_list
    
let rec sum_cells lst = match lst with [] -> 0 | 
           (_,_,m)::tail ->  m + (sum_cells tail)

let get_diffusable_cells matrix_diffusion = 
    let lst = ref [] in
     for i = 0 to (matrix_dim-1) do 
      for j = 0 to (matrix_dim-1) do
         if matrix_diffusion.(i).(j) != [] then lst := (i,j, sum_cells  matrix_diffusion.(i).(j) )::!lst
      done
    done;
    !lst

let print_triple (a,b,c) = print_string ((string_of_int a) ^ "," ^ (string_of_int b) ^ "," ^ (string_of_int c) ^ " ~ ")

let update_height  matrix_model  triple_lst = 
         let my_compare (_,_,a) (_,_,b) = if a > b then -1 else 1 
      in let rec uh lst = match lst with [] -> [] |
          (a,b,_)::tail -> (a,b,second matrix_model.(a).(b) )::(uh tail) 
      in let result_lst = List.sort my_compare (uh triple_lst)
      in (* List.iter print_triple result_lst;
         print_string "\n"; *)
         result_lst     

let rec get_first_n_sum  n  fertile_cells_lst  sum = 
          if n = 0 then sum
                   else match fertile_cells_lst with [] -> failwith "Match Error" 
                        | (_,_,c)::tail -> get_first_n_sum  (n-1)  tail  (sum+c)  

(* ######################################################### *)
(* ######################################################### *)
(* ~~~~~~~~~~~~~~~~~~~~~ Propensities ~~~~~~~~~~~~~~~~~~~~~~ *) 

let compute_propensity_diffusion  diffusable_cells_list  = 
    let rec cpd lst =  match lst with [] -> 0 |
           (x,y,magnitude)::tail -> 
           (* print_string ((string_of_int magnitude) ^"\n"); *)
           magnitude + (cpd tail) in
    cpd diffusable_cells_list      

let rec get_Nth k index lst = if k = index
                              then List.hd lst
                              else get_Nth (k+1) index (List.tl lst)         

let rec element_in_interval  interval  cell_lst = match cell_lst with [] -> failwith "Pick error" |
      (a,b,h)::tail ->  if interval <= h then (a,b,h) else element_in_interval  (interval-h)  tail

(* ######################################################### *)
(* ---------------------- Diffusion ------------------------ *)  

let print_ds (a,b,c) = print_string ((string_of_int a) ^ "," ^ (string_of_int b) ^ "," ^ (string_of_int c) ^ " ")  

let pick_diffusion  diffusable_cells  matrix_diffusion = 
     let sum = sum_cells diffusable_cells in
     let from_index = (Random.int sum)+1 in
     let (x_from,y_from,interval_from) = element_in_interval  from_index   diffusable_cells in
     let possible_diffusions_list = matrix_diffusion.(x_from).(y_from) in
     let diffusion_index_to = (Random.int  interval_from)+1  in
     let (x_to,y_to,c) = element_in_interval  diffusion_index_to   possible_diffusions_list in
     ((x_from,y_from),(x_to,y_to))
     (* try  *)
     (* with  _ ->  *)
     (*
     print_string ("sum: " ^ (string_of_int (sum)) ^ "\n"); 
     print_string ("Diffusable cells: "); 
     List.iter print_ds diffusable_cells;
     print_string "\n";
     print_string ("From index: " ^ (string_of_int (from_index)) ^ "\n");
     print_string ("Picked this one: ");
     print_ds (x_from,y_from,interval_from);
     print_string "\n";
     print_string ("Next diffusable cells: "); 
     List.iter print_ds possible_diffusions_list;
     print_string "\n";
     print_string ("To index: " ^ (string_of_int (diffusion_index_to)) ^ "\n");
     print_string ("Next diffusion: ");
     print_ds (x_to,y_to,c);
     print_string "\n"; 
     exit 0;
     *)
     
       

  let model_update_after_diffusion  matrx  ((x,y),(x_new,y_new)) = 
      let _ = if not (in_boundaries x_new  y_new)  then failwith "Error 1"        
   in let _ = if second matrx.(x).(y) <= second matrx.(x_new).(y_new) then failwith "Error 2"
   in matrx.(x).(y) <- (first matrx.(x).(y),(second matrx.(x).(y)) -1);
      matrx.(x_new).(y_new) <- (first matrx.(x_new).(y_new),(second matrx.(x_new).(y_new)) +1)
 
  let diffusion_model_update_after_diffusion  matrix  matrix_diffusion ((x_diffuse_from,y_diffuse_from),(x_diffuse_to,y_diffuse_to)) =
  (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ *) 
  let cells_to_explore = [ (x_diffuse_from,y_diffuse_from);        (x_diffuse_to,      y_diffuse_to);  
                           (x_diffuse_from+1,  y_diffuse_from);    (x_diffuse_to+1,    y_diffuse_to);    
                           (x_diffuse_from-1,  y_diffuse_from);    (x_diffuse_to-1,    y_diffuse_to);    
                           (x_diffuse_from  ,  y_diffuse_from+1);  (x_diffuse_to  ,    y_diffuse_to+1);
                           (x_diffuse_from  ,  y_diffuse_from-1);  (x_diffuse_to  ,    y_diffuse_to-1)] 
  (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ *) 
  in set_model_diff_after_event_for_cell_list  matrix  matrix_diffusion  cells_to_explore                        

  
(* ######################################################### *)
(* ------------------------ Budding ------------------------ *) 

let pick_budding fertile_cells_list = 
  let sum = sum_cells fertile_cells_list in
  let budding_index = (Random.int sum)+1 in
  let budding_cell = element_in_interval  budding_index   fertile_cells_list in
  budding_cell
     

let model_budding_update matrx (x_new,y_new,_height) = 
  let _ = if  not (in_boundaries x_new  y_new) then failwith "Error 5" 
in let _ = if first matrx.(x_new).(y_new) = 1 then failwith "Error 7"
in matrx.(x_new).(y_new) <- (1,(second matrx.(x_new).(y_new)))

(* ######################################################### *)
(* ---------------------- Deposition ----------------------- *)  

let pick_deposition_polyp polyp_list = 
  let this_length = List.length polyp_list in
  let deposition_polyp_index = Random.int this_length in
  get_Nth  0  deposition_polyp_index  polyp_list

let model_deposit_update matrx (x_new,y_new) = 
  let _ = if not (in_boundaries x_new  y_new)  then failwith "Error 3"        
  in matrx.(x_new).(y_new) <- (first matrx.(x_new).(y_new),(second matrx.(x_new).(y_new)) +1)

let diffusion_model_update_after_deposition  matrix  matrix_diffusion (x_diffuse_to,y_diffuse_to) =
    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ *) 
    let cells_to_explore = [ (x_diffuse_to,      y_diffuse_to);  
                             (x_diffuse_to+1,    y_diffuse_to);    
                             (x_diffuse_to-1,    y_diffuse_to);    
                             (x_diffuse_to  ,    y_diffuse_to+1);
                             (x_diffuse_to  ,    y_diffuse_to-1)] 
    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~~ *) 
    in set_model_diff_after_event_for_cell_list  matrix  matrix_diffusion  cells_to_explore                      
         
(* ######################################################### *)

let initiate_file file_name = 
  let output_file = file_name  in
  let channel1 = open_out output_file in
  let _ = output_string channel1  "" in
  let _ = close_out channel1 in ()

let print_to_file_fluxes file_name events_list =
  let output_file = file_name  in
  let channel1 = open_out_gen [Open_append; Open_creat] 0o666 file_name in
  (* let channel1 = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 file_name in *)
  (* let channel1 = open_out output_file in *)
  (* first line of the file *)
  let print_event_to_file (time,(event_id,(x0,y0),(x1,y1))) = 
            output_string channel1   ((string_of_float time) ^ " , " ^  (string_of_int event_id) ^ " , " ^  (string_of_int x0) ^
                                 " , " ^  (string_of_int y0) ^ " , " ^  (string_of_int x1) ^ " , " ^  (string_of_int y1) ^ ";\n") in 
  List.iter print_event_to_file events_list;
  close_out channel1

  
let run x y z  budding_exp  end_time_string  output_file_index = 
    let end_time = float_of_string (end_time_string) in
    let current_time = ref 0.0 in
    let next = ref 0 in 
    let sim = ref [(0.,(2,(0,0),(50,50)))] in
    (* -------------------------- *)
    (*     state of each cell     *)
    let model = Array.make_matrix matrix_dim matrix_dim (0,0) in
    (* ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
    (*     possible diffusions from each cell      *)
    let model_diffusion = Array.make_matrix matrix_dim matrix_dim [] in
    let diffusable_cells = ref [] in
    (* -------------------------- *)
    (*  list of cells with a polyp *)
    let polyp_cells = ref [] in
    let polyp_count = ref 1 in 
    (* -------------------------- *)
    (*  list of fertile cells where budding can occur  *)
    let fertile_cells = ref [] in
    (* -------------------------- *)
    (* let prnt_state cell_list = print_state  cell_list   model   model_diffusion !polyp_cells !fertile_cells !diffusable_cells in  *)
    (* vvvvvvvvvvvvvvvvvvvvvvvvvv *)
    let diffusion_rate = float_of_string x in
    let budding_rate = float_of_string y in
    let deposition_rate = float_of_string z in
    (* -------------------------- *)
    let budding_exponent = float_of_string budding_exp in
    (* -------------------------- *)
    (*   initialization calls     *)
    let _ = model.(50).(50) <- (1,0) in
    let _ = model_diffusion.(50).(50) <- [] in
    let _ = diffusable_cells := [] in 
    let _ = polyp_cells := [(50,50)] in
    let _ = fertile_cells := [] in
    (* -------------------------- *) 
    let get_diffusion_propensity diffusable_cells_lst = (float_of_int (compute_propensity_diffusion diffusable_cells_lst)) *. diffusion_rate in 
    let get_budding_propensity polyp_cells_lst fertile_cells_lst = 
         let min_count = min  (List.length polyp_cells_lst)  (List.length fertile_cells_lst)  in 
         let first_n_sum = get_first_n_sum  min_count  fertile_cells_lst  0 in 
          (float_of_int first_n_sum) *. budding_rate /. ( (float_of_int !polyp_count) ** budding_exponent ) in
    let get_deposition_propensity polyp_cells_lst = (float_of_int (List.length polyp_cells_lst)) *.  deposition_rate in
    (* -------------------------- *)
    (* let _ =  print_string "\n" in  *)
    (* -------------------------- *)
    let a_j = [| 0.; 
                get_diffusion_propensity !diffusable_cells;
                get_budding_propensity !polyp_cells !fertile_cells; 
                get_deposition_propensity !polyp_cells |] in
    let update_a0 () = a_j.(0) <- a_j.(1) +. a_j.(2) +. a_j.(3) in 
    let _ = Random.self_init () in
    (* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *)
    (* ---------------------------- *)
    let update_diffuse () = 
          let (diffuse_from,diffuse_to) = pick_diffusion !diffusable_cells  model_diffusion in 
          let () =  (* update model *) model_update_after_diffusion  model  (diffuse_from,diffuse_to) in 
          let () = (* update diffusion-model *) diffusion_model_update_after_diffusion  model  model_diffusion  (diffuse_from,diffuse_to)  in
          let () = (* diffusable cells *) diffusable_cells := get_diffusable_cells model_diffusion in  
          let () = (* polyp cells // non-introduced *) () in 
          let () = (* fertile cells *) fertile_cells := update_height model (set_fertile_cells_after_event  model (conditional_fertile_remove  model  diffuse_from  !fertile_cells)  diffuse_to) in 
          let () = (* update a_j *) a_j.(1) <- get_diffusion_propensity !diffusable_cells in
          let () = (* update a_j *) a_j.(2) <- get_budding_propensity !polyp_cells !fertile_cells in 
          (1,diffuse_from,diffuse_to)
          in  
    (* ---------------------------- *)
    let update_budding () = 
          let first_two (a,b,c) = (a,b) in
          let budding_cell_triple = pick_budding !fertile_cells in 
          let budding_cell = first_two budding_cell_triple in 
          let () = (* update model *)  model_budding_update  model  budding_cell_triple  in 
          let () = (* update model diffusion ~~ remains unchanged *) () in
          let () = (* polyp cells *) polyp_cells := budding_cell::!polyp_cells  in
          let () = (* polyp count *) polyp_count := !polyp_count + 1 in  
          let () = (* fertile cells *) fertile_cells := update_height model (remove_from_list  budding_cell  !fertile_cells) in 
          let () = (* diffusable cells ~~ remains unchanged *) () in 
          let () = (* update a_j *) a_j.(2) <- get_budding_propensity !polyp_cells !fertile_cells;
                                    a_j.(3) <- get_deposition_propensity !polyp_cells in 
          (2,(0,0),budding_cell)
          in 
    (* ---------------------------- *)
    let update_deposition () =  
          let deposition_cell = pick_deposition_polyp !polyp_cells in 
          let () = (* update model *)  model_deposit_update model deposition_cell  in 
          let () = (* update model diffusion *) diffusion_model_update_after_deposition  model  model_diffusion deposition_cell  in 
          let () = (* diffusable cells *) diffusable_cells := get_diffusable_cells model_diffusion in  
          let () = (* polyp cells ~~ non-introduced *) () in 
          let () = (* fertile cells *) fertile_cells := update_height model  !fertile_cells in 
          let () = (* update a_j *) a_j.(1) <- get_diffusion_propensity !diffusable_cells in 
          (3,(0,0),deposition_cell)
          in
    (* ---------------------------- *)
    let compute_event () = match !next with 
                        1 -> update_diffuse ()
                      | 2 -> update_budding ()
                      | 3 -> update_deposition ()
                      | _ -> (0,(0,0),(0,0)) in
    (* ---------------------------- *)
    let count = ref 0 in
    (* ---------------------------- *)
    let this_file_name =  "sim_output" ^ output_file_index ^ ".txt" in
    let _ = initiate_file  this_file_name  in
    while (!current_time <= end_time ) (* && !polyp_count < 100)  *)
    (* for n =0 to 10 *)
    do 
       (* List.iter print_triple !fertile_cells;
       print_string "\n"; *)
       count := !count + 1; 
       if  (!count mod 50000) = 0 
       then (print_to_file_fluxes   this_file_name   !sim;
             sim := []);  
       (* (print_string ("."); flush stdout); *)
       update_a0 ();
       (* print_string "\n";  *)
       (* for k = 0  to 3 do print_string ((string_of_int k) ^ " " ^ (string_of_float a_j.(k)) ^ "\n") done; *)
       next := next_action a_j;
       (* print_string ("\nnext: " ^(string_of_int !next) ^ "\n"); *)
       current_time := !current_time +. (compute_tau a_j.(0));
       let event = compute_event () 
        (* try compute_event () *)
        (* with _ -> prnt_state [(50,50);(51,50);(50,51);(49,50);(50,49);(52,50);(50,52);(48,50);(50,48)];(0,(0,0),(0,0)) *)
       in sim := !sim @ [(!current_time,event)]
       (* ()   *)
       (* print_string ("\nTime:" ^ (string_of_float !current_time) ^"\n");  *)
       (* prnt_state [(50,50);(51,50);(50,51);(49,50);(50,49);(52,50);(50,52);(48,50);(50,48)] *)
    done;
    (* print_string "\n"; *)
    print_to_file_fluxes  this_file_name !sim
   
let _ = run Sys.argv.(1) Sys.argv.(2) Sys.argv.(3) Sys.argv.(4) Sys.argv.(5) Sys.argv.(6)
     
(* 
The parameters of "run" above are
1) diffusion rate
2) budding_rate
3) deposition_rate
4) budding_exponent
5) simulation end-time
6) output file suffix
*)


 (*  
 For HPC que:
  qstat  -t -u $USER
  
  For deleting a job:
   qdel 
 *)

(* ################################################# *)
(* ################################################# *)
(* ################################################# *)
