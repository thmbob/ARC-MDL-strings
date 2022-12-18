open Types_utilities

let xp_string (print : Xprint.t) (s : string) =
  print#string "<pre class=\"inline\">";
  print#string s;
  print#string "</pre>"
let pp_string = Xprint.to_stdout xp_string

let xp_var (print : Xprint.t) (v : var) =
  print#string "[";
  let rec aux v =
    match v with
    | [] -> ()
    | t::[] -> print#string (string_of_int t)
    | t::q -> print#string (string_of_int t); print#string ";"; aux q
  in aux v;
  print#string "]"

let pp_row_path = Xprint.to_stdout xp_var

let rec xp_row_model (print : Xprint.t) lm =
  List.iteri
    (fun i m ->
      if i > 0 then print#string "</br>";
      xp_cell_model print [i] m)
    lm
and xp_cell_model (print : Xprint.t) (v : var) (cm : cell_model) = 
  print#string "[";
  let rec aux cm i =
    match cm with
    | [] -> ()
    | t::[] -> xp_token_model print (v@[i]) t
    | t::q -> xp_token_model print (v@[i]) t; print#string ";"; aux q (i+1)
  in aux cm 0;
  print#string "]"
and xp_token_model print (v : var) = function
  | NilToken -> ()
  | AnyToken ->
     print#string "<span class=\"model-any\">*</span>"
  | ConstToken s ->
     print#string "<span class=\"model-const\">";
     xp_string print s;
     print#string "</span>"
  | RegexToken re ->
     print#string "<span class=\"model-regex\">";
     print#string "?";
     xp_var print v;
     print#string " : ";
     xp_regex_model print re;
     print#string "</span>"
  | ExprToken e ->
     print#string "<span class=\"model-expr\">";
     Expr.xp_expr xp_var print e;
     print#string "</span>"
  | ListToken t ->
     print#string "<span class=\"model-list\">";
     print#string "List(";
     xp_token_model print v t;
     print#string ")";
     print#string "</span>" 
and xp_regex_model print = function
  | Alphas -> print#string "Alphas"
  | Nums -> print#string "Digits"
  | Letters -> print#string "Letters"
  | Date -> print#string "Date"
  | Blank -> print#string "Blank"

let pp_row_model m = Xprint.to_stdout (xp_row_model) m
let string_of_row_model m = Xprint.to_string (xp_row_model) m
                    
let rec xp_row_data (print : Xprint.t) ld =
  List.iteri
    (fun i d ->
      if i > 0 then print#string "</br>";
      xp_cell_data print d)
    ld
and xp_cell_data (print : Xprint.t) (cd : cell_data) =
  List.iter (fun td -> xp_token_data print td) cd
and xp_token_data print = function
  | DNil -> ()
  | DAny s ->
     print#string "<span class=\"data-any\">";
     xp_string print s;
     print#string "</span>"
  | DString s ->
     print#string "<span class=\"data-token\">";
     xp_string print s;
     print#string "</span>"
  | DList l ->
     print#string "<span class=\"data-list\">";
     print#string "list[";
     xp_cell_data print l;
     print#string "]";
     print#string "</span>"

let pp_row_data = Xprint.to_stdout xp_row_data
let string_of_row_data = Xprint.to_string xp_row_data

  (* refinement *)
let xp_cell_refinement (print : Xprint.t) (r : cell_refinement) =
  let RCell r = r in 
  match r with
  | RMerge -> print#string "<span class=\"refinement\">Merge</span>"
  | RToken tok -> xp_token_model print [] tok

let pp_cell_refinement = Xprint.to_stdout xp_cell_refinement

let rec xp_refinement (print : Xprint.t) = function
  | RInit -> print#string "init"
  | Rinput (v,ri,dl') -> xp_refinement_aux print " In." v ri dl' "i"
  | Routput (v,ro, dl') -> xp_refinement_aux print " Out." v ro dl' "o"
and xp_refinement_aux print in_out v r dl' i_o =
  print#string (Printf.sprintf " / ~%.3f%s)  " dl' i_o);
  print#string in_out;
  xp_var print v;
  print#string " ← ";
  xp_cell_refinement print r
let pp_refinement = Xprint.to_stdout xp_refinement
let string_of_refinement = Xprint.to_string xp_refinement

(* pairs/examples *)
let xp_model (print : Xprint.t) (m : model) =
  xp_row_model print m.input_model;
  print#string " ➜ ";
  xp_row_model print m.output_model
let pp_model = Xprint.to_stdout xp_model
let string_of_model = Xprint.to_string xp_model
