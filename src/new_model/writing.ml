open Types_utilities
open Environment
open Printing

(* generate *)
(*
let rec row_generate (lm : row_model) : row_data =
  List.map token_generate lm
and token_generate : token_model -> token_data = function
  | NilToken -> DNil
  | AnyToken -> DAny "..."
  | ConstToken s -> DToken s
  | RegexToken re -> DToken (regex_generate re)
  | ExprToken _ -> assert false
  | ListToken cm -> DList [token_generate cm]
and regex_generate : regex_model -> string = function
  | Alphas -> "X1_Y2"
  | Nums -> "123"
  | Letters -> "Abc"
  | Date -> "d/m/y"
  | Blank -> " \\t"

(* writing *)

let write_row ~(env : env) (m : row_model) : (string list, exn) Result.t = Common.prof "Model.write_doc" (fun () ->
  let| m' = row_apply m env in
  let d = row_generate m' in
  let ls = row_of_row_data d in
  Result.Ok ls)
*)

let rec apply_cell_refinement (r : cell_refinement) (v : var) (lm : row_model) : row_model * int =
  Printf.printf "Writing.apply_cell_refinement r:"; print_cell_refinement r; Printf.printf " v:"; print_var v; Printf.printf " lm:"; print_row_model lm; print_newline ();
  let RCell r = r in
  match v with
  | [] -> assert false
  | i::q -> 
      Printf.printf "length(lm)=%d>%d=i\n" (List.length lm) i;
      assert (List.length lm > i);
      let cmf, cm::cmq = List.cut_at_n lm i in
      let cm, shift = apply_token_refinement r q cm in
      print_string "new cell model:"; print_cell_model cm; print_newline ();
      cmf@(cm::cmq), shift

and apply_token_refinement (r : token_refinement) (v : var) (cm : cell_model) : cell_model * int =
  Printf.printf "Writing.apply_token_refinement v:"; print_int_list v; Printf.printf "r: "; print_token_refinement r; Printf.printf " cm:"; print_cell_model cm; print_newline ();
  assert (v <> []);
  let i::_ = v in
  let f, q = List.cut_at_n cm i in
  Printf.printf "f:"; print_cell_model f; Printf.printf "q:"; print_cell_model q; print_newline ();
  let aux_token l =
    match l, r with
    | AnyToken::q, RToken NilToken -> q, -1
    | AnyToken::q, RToken tok -> AnyToken::tok::AnyToken::q, 2
    | (RegexToken _)::q, RToken tok -> tok::q, 0
    | (ListToken _)::q, RToken tok -> tok::q, 0
    | (RegexToken m)::_::_::q, RMerge -> (ListToken (RegexToken m))::q, -2
    | (ListToken m)::_::_::q, RMerge -> (ListToken m)::q, -2
    | _ -> Printf.printf "Impossible to apply the given token refinement\n"; assert false in
  let end_new_model, shift = aux_token q in
  Printf.printf "end_new_model:"; print_cell_model end_new_model; print_newline ();
  f@end_new_model, shift

let apply_refinement (r : refinement) (m : model) : (refinement * model) result =
  Printf.printf "Writing.apply_refinement\n";
  match r with
  | RInit -> Result.Error (Failure "apply_refinement")
  | Rinput (p,ri,dl') ->
     let inmo, shift = apply_cell_refinement ri p m.input_model in
     Result.Ok (r, {input_model = inmo; output_model = row_shift_ref m.output_model p shift})
  | Routput (p,ro,dl') ->
     let outmo, shift = apply_cell_refinement ro p m.input_model in
     Result.Ok (r, {m with output_model = outmo})


let rec row_generate (lm : row_model) : row_data =
  Printf.printf "Writing.row_generate\n";
  List.map cell_generate lm
and cell_generate (cm : cell_model) : cell_data =
  Printf.printf "Writing.cell_generate\n";
  List.map token_generate cm
and token_generate : token_model -> token_data = function
  | NilToken -> DNil
  | AnyToken -> DAny "..."
  | ConstToken s -> DString s
  | RegexToken re -> DString (regex_generate re)
  | ExprToken _ -> assert false
  | ListToken m -> DList (List.init 1 (fun _ -> token_generate m))
and regex_generate : regex_model -> string = function
  | Alphas -> "X1_Y2"
  | Nums -> "123"
  | Letters -> "Abc"
  | Date -> "d/m/y"
  | Blank -> " \\t"

let write_row ~(env : env) (m : row_model) : (string list, exn) Result.t = 
  Common.prof "Model.write_doc" 
  (fun () ->
    let| m' = row_apply m env in
    let d = row_generate m' in
    let ls = row_of_row_data d in
    Result.Ok ls)

