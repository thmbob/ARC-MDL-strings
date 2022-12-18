open Types_utilities

let print_list print l =
  print_string "[";
  let rec aux l =
    match l with
    | [] -> ()
    | t::[] -> print t
    | t::q -> print t; print_string ";"; aux q
  in aux l;
  print_string "]"

let rec print_expr (print_var : 'var -> unit) : 'var Expr.expr -> unit = function
  | `Lambda -> print_string "lambda"
  | `Ref p -> print_string "!"; print_var  p
  | `Unary (op, e) -> print_op op; print_string "("; print_expr print_var e; print_string ")";
  | `Binary (op, e1, e2) -> print_op op; print_string "("; print_expr print_var e1; print_string ", "; print_expr print_var e2; print_string ")";
and print_exprset (print_var : 'var -> unit) : 'var Expr.exprset -> unit = function
  | [] -> ()
  | (`Lambda)::q -> print_string "lambda"
  | (`Ref p)::q -> print_string "!"; print_var  p
  | (`Unary (op, e))::q -> print_op op; print_string "["; print_exprset print_var e; print_string "]";
  | (`Binary (op, e1, e2))::q -> print_op op; print_string "["; print_exprset print_var e1; print_string ", "; print_exprset print_var e2; print_string "]";
and print_op = function
  | `Uppercase -> print_string "uppercase"
  | `Lowercase -> print_string "lowercase"
  | `Initial -> print_string "initial"
  | `Length -> print_string "length"
  | `Concat -> print_string "concat"
  | `Mirror -> print_string "mirror"
  | `String_of_int -> print_string "string_of_int"
  | `Date_of_string -> print_string "date_of_string"
  | `Get_day -> print_string "get_day"
  | `Get_month_in_letters -> print_string "get_month_in_let"
  | `Get_month_in_numbers -> print_string "get_month_in_num"
  | `Get_year -> print_string "get_year"
  | `Time_of_string -> print_string "time_of_string("
  | `Get_hours -> print_string "get_hours"
  | `Get_minutes -> print_string "get_minutes"
  | `Get_seconds -> print_string "get_seconds"
  | `Concat_list -> print_string "concat_list"
  | `String_list_map -> print_string "string_list_map"
  | `String_of_list -> print_string "string_of_list"

let print_int_list = print_list print_int
let print_var = print_int_list

let print_value e =
  let r = Expr.string_of_value e in 
  let s = match r with Result.Ok(s) -> s | Result.Error(_) -> "Err" in
  print_string s  

let rec print_bindings (index : (var * Expr.value) list) = 
  match index with
  | [] -> print_newline ()
  | (v,va)::q -> print_var v; print_string ":"; print_value va; print_string " ; "; print_bindings q

let rec print_index (index : (int list) Expr.index) =
  Printf.printf "indexes : \n";
  Expr.IntIndex.iter (fun i e -> (Printf.printf "i:%d " i; print_exprset print_var e; Printf.printf " ; ")) index.by_int;
  print_newline ();
  Expr.StringIndex.iter (fun s e -> Printf.printf "s:%s " s; print_exprset print_var e; Printf.printf " ; ") index.by_string;
  print_newline ();
  Expr.DateIndex.iter (fun d e -> let j,m,a = d in Printf.printf "%d/%d/%d " j m a; print_exprset print_var e; Printf.printf " ; ") index.by_date;
  print_newline ();
  Expr.TimeIndex.iter (fun t e -> let h,m,s = t in Printf.printf "%d:%d:%d " h m s; print_exprset print_var e; Printf.printf " ; ") index.by_time;
  print_newline ()

let rec print_row_model (cl : row_model) =
  print_string "[";
  let rec aux cl =
    match cl with
      | [] -> ()
      | t::[] -> print_cell_model t
      | t::q -> (print_cell_model t; print_string ";"; aux q)
  in aux cl;
  print_string "]"
and print_cell_model (tl : cell_model) =
print_string "[";
let rec aux tl =
  match tl with
    | [] -> ()
    | t::[] -> print_token_model t
    | t::q -> (print_token_model t; print_string ";"; aux q)
in aux tl;
print_string "]"
and print_token_model (m : token_model) =
  match m with
  | NilToken -> print_string "Nil"
  | AnyToken -> print_string "Any"
  | RegexToken re -> print_string "Regex("; print_regex_model re; print_string")"
  | ConstToken s -> print_string "Const("; print_string s; print_string ")"
  | ListToken m -> print_string "List("; print_token_model m; print_string ")"
  | ExprToken e -> print_expr print_var e
and print_regex_model = function
  | Alphas -> print_string "Alphas"
  | Letters -> print_string "Letters"
  | Nums -> print_string "Nums"
  | Date -> print_string "Date"
  | Blank -> print_string "Blank"

let rec print_refinement r =
  match r with
  | RInit -> print_string "RInit"
  | Rinput (v,c,dl) -> print_string "RInput("; print_int_list v; print_string ", "; print_cell_refinement c; print_string ", "; print_float dl; print_string ")" (* estimated result DL *)
  | Routput (v,c,dl) -> print_string "ROutput("; print_int_list v; print_string ", "; print_cell_refinement c; print_string ", "; print_float dl; print_string ")" (* estimated result DL *)
and print_cell_refinement r = 
  match r with
  | RCell r -> print_string "RCell("; print_token_refinement r; print_string ")"
and print_token_refinement r =
  match r with
  | RToken m -> print_string "RToken("; print_token_model m; print_string ")"
  | RMerge -> print_string "Rmerge"

let rec print_token_data (dt : token_data) =
  match dt with
  | DNil -> print_string "(DNil)"
  | DAny s -> print_string "DAny("; print_string s; print_string ")"
  | DString s -> print_string "DString("; print_string s; print_string ")"
  | DList l -> print_string "DList("; print_cell_data l; print_string ")"
and print_cell_data l =
  print_string "[";
  let rec aux l =
    match l with 
    | [] -> ()
    | t::[] -> print_token_data t
    | t::q -> print_token_data t; print_string ";"; aux q
  in aux l;
  print_string "]"; print_newline ()
and print_row_data ld =
print_string "[";
let rec aux l =
  match l with 
  | [] -> ()
  | t::[] -> print_cell_data t
  | t::q -> print_cell_data t; print_string ";"; aux q
in aux ld;
print_string "]"; print_newline ()



