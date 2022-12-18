open Types_utilities
open Printing

(* get and apply *)

let rec row_of_row_data (ld : row_data) : string list =
  Printf.printf "Environment.row_of_row_data\n";
  List.fold_right (fun cd l -> (cell_of_cell_data cd)::l) ld []
and cell_of_cell_data (cd : cell_data) : string =
  Printf.printf "Environment.cell_of_cell_data\n";
  List.fold_right (fun td s -> (token_of_token_data td)^s) cd ""
and token_of_token_data (td : token_data) : string =
  Printf.printf "Environment.cell_of_cell_data : "; print_token_data td; print_newline ();
  match td with
  | DNil -> ""
  | DAny s -> s
  | DList l -> cell_of_cell_data l
  | DString s -> s
              
let rec row_find (v : var) (ld : row_data) : Expr.value result =
  Printf.printf "Environment.row_find\n";
  match v with
  | [] -> assert false
  | i::[] -> Result.Ok (`String (cell_of_cell_data (List.nth ld i)))
  | i::q -> cell_find q (List.nth ld i)
and cell_find (v : var) (cd : cell_data) : Expr.value result =
  Printf.printf "Environment.cell_find\n";
  match v with
  | [] -> Printf.printf "empty list\n"; assert false
  | i::[] -> Printf.printf "get i:%d/ld:%d\n" i (List.length cd); token_find (List.nth cd i)
  | i::q -> let DList cd = List.nth cd i in cell_find q cd
and token_find (td : token_data) : Expr.value result =
  match td with
  | DString s -> Result.Ok(`String s)
  | DList cd -> Result.Ok(`List (Expr.get_even_elements (fun td -> match td with DString s -> `String s | _ -> assert false) cd))
  | _ -> Result.Error (Invalid_argument "data type not valid")

let rec row_bindings (ld : row_data) : (var * Expr.value) list =
  Printf.printf "Environment.row_bindings\n";
  List.flatten (List.mapi (fun i -> cell_bindings [i]) ld)
and cell_bindings (v : var) (cd : cell_data) : (var * Expr.value) list =
  print_string "Environment.cell_bindings : v:"; print_int_list v; print_string " cd:"; print_cell_data cd;
  let rec aux_bindings v index cd =
    match cd with
    | [] -> []
    | DNil::q -> aux_bindings v (index+1) q
    | (DAny _)::q -> aux_bindings v (index+1) q
    | (DList cd)::q -> (v@[index], `List (Expr.get_even_elements (fun td -> match td with DString s -> `String s | _ -> assert false) cd))::(aux_bindings v (index+1) q)
    | (DString s)::q -> (v@[index], `String s)::(aux_bindings v (index+1) q)
  in let r = aux_bindings v 0 cd in
  Printf.printf "Env after bindings: ";
  print_bindings r;
  r

let rec row_shift_ref (lm : row_model) (v : var) (shift : int)  : row_model =
  Printf.printf "Environment.row_shift_ref m:"; print_row_model lm; Printf.printf " v:"; print_var v; Printf.printf " shift:%d\n" shift;
  List.map (cell_shift_ref v shift) lm
and cell_shift_ref (v : var) (shift : int) (cm : cell_model) : cell_model =
Printf.printf "Environment.cell_shift_ref m:"; print_cell_model cm; Printf.printf " v:"; print_var v; Printf.printf " shift:%d\n" shift;
  List.map (token_shift_ref v shift) cm
and token_shift_ref (v : var) (shift : int) (tm : token_model) : token_model =
Printf.printf "Environment.token_shift_ref m:"; print_token_model tm; Printf.printf " v:"; print_var v; Printf.printf " shift:%d\n" shift;
  match tm with
  | ExprToken e -> ExprToken(modify_ref v shift e)
  | ListToken lt -> ListToken(token_shift_ref v shift lt)
  | _ -> tm
and modify_ref (v : var) (shift : int) (e : expr) : expr =
  Printf.printf "Environment.modify_ref e:"; print_expr print_var e; Printf.printf " v:"; print_var v; Printf.printf " shift:%d\n" shift;
  match e with
  | `Lambda -> `Lambda
  | `Ref w -> `Ref(correct_ref_if v w shift) (* inverser v et w ici m'a couté ~4h de débogages :( )*)
  | `Unary (op, e) -> `Unary (op, modify_ref v shift e)
  | `Binary (op, e1, e2) -> `Binary (op, modify_ref v shift e1, modify_ref v shift e2)
and correct_ref_if (v : var) (w : var) (shift : int) : var =
  Printf.printf "Environment.correct_ref_if v:"; print_var v; Printf.printf " w:"; print_var w; Printf.printf " shift:%d\n" shift;
  match v, w with
  | i::[], j::[] when i<j -> [j+shift]
  | i::qv, j::qw when i=j -> j::(correct_ref_if qv qw shift)
  | _, _ -> w

let rec row_apply (lm : row_model) (env : env) : row_model result =
  Printf.printf "Environment.row_apply\n";
  list_map_result
    (fun m -> cell_apply m env)
    lm
and cell_apply (cm : cell_model) (env : env) : cell_model result =
  Printf.printf "Environment.cell_apply\n";
  list_map_result 
    (fun m -> token_apply m env) 
    cm
and token_apply (m : token_model) (env : env) : token_model result =
  Printf.printf "Environment.token_apply\n";
  match m with
  | NilToken -> Result.Ok NilToken
  | AnyToken -> Result.Ok AnyToken
  | ConstToken s -> Result.Ok (ConstToken s)
  | RegexToken re -> Result.Ok (RegexToken re)
  | ExprToken e -> (
     let| v = Expr.eval (fun v -> Printf.printf "Environment.token_apply.row_find_lambda v:"; print_var v; print_newline (); row_find v env) e in
     let r = Expr.string_of_value v in 
     match r with
     | Result.Ok s -> Result.Ok (ConstToken s)
     | Result.Error e -> (let| l = Expr.string_list_of_value v in
                          let s = Expr.Funct.string_of_list l in
                          Result.Ok (ConstToken s))
                    )
                        
  | ListToken tm -> let| r = token_apply tm env in
                    Result.Ok (ListToken r)