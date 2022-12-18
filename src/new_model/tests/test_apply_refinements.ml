open Types_utilities
open Parsing
open Printing
open Environment
open Dl_compute
open Writing

module Myseq = struct
  include Myseq
  let uncons seq = 
    match seq () with
    | Nil -> None
    | Cons(h,t) -> Some (h, t)
end

let path m s = 
  let enc_m = row_encoder m in
  let dl_m = dl_row_model ~env:m m in
  Printf.printf "parse seq\n";
  let parse_seq = row_parse m s in
  let parse_seq2 = row_parse m s in
  Printf.printf "\nParsing result:\n";
  Myseq.iter print_row_data parse_seq;
  let var_seq = Myseq.map (fun x -> Printf.printf "var_seq_map\n"; row_bindings x) parse_seq2 in
  Printf.printf "\nEnvironment :\n";
  Myseq.iter (fun l -> let rec aux l = 
                         match l with 
                           | [] -> print_newline () 
                           | (v,e)::q -> (print_var v; print_string ":"; 
                                             let r = Expr.string_of_value e in 
                                             let s = match r with Result.Ok(s) -> s | Result.Error(_) -> "" in
                                             print_string s; 
                                             print_string " ; "; 
                                             aux q) in 
                      aux l) 
             var_seq;
  Printf.printf "DL :\n";
  Myseq.iter (fun rd -> let dl_d = enc_m rd in Printf.printf "m:%f d:%f sum:%f" dl_m dl_d (dl_m+.dl_d)) parse_seq2;
  Printf.printf "\n"

let rec complete_example init_model refinements s =
  Printf.printf "\n\n";
  print_row_model init_model;
  path init_model s;
  match refinements with
    | [] -> ()
    | t::q -> let v,r = t in
              print_string "refinement:"; print_cell_refinement r; print_string " pos:"; print_int_list v; print_newline (); 
              complete_example (apply_cell_refinement r v init_model) q s
  
let () =
  let ls = [" 123 75  17/01/22";"1677   099 78"] in
  (* [AnyToken;RegexToken(Letters);ConstToken(" ");ListToken(RegexToken(Nums));RegexToken(Blank);RegexToken(Date)] *)
  let rl = [([0;0], RCell(RToken(RegexToken(Date))));     (* [*;Date;*] *)
            ([0;0], RCell(RToken(ConstToken(" "))));      (* [ *;' ';*;Date;*] *)
            ([0;2], RCell(RToken(RegexToken(Nums))));     (* [ *;' ';*;Nums;*;Date;*] *)
            ([0;4], RCell(RToken(RegexToken(Nums))));     (* [ *;' ';*;Nums;*;Nums;*;Date;*] *)
            ([0;0], RCell(RToken(NilToken)));             (* [' ';*;Nums;*;Nums;*;Date;*] *)
            ([0;1], RCell(RToken(NilToken)));             (* [' ';Nums;*;Nums;*;Date;*] *)
            ([0;2], RCell(RToken(RegexToken(Blank))));    (* [' ';Nums;*;Blank;*;Nums;*;Date;*] *)
            ([0;6], RCell(RToken(RegexToken(Blank))));    (* [' ';Nums;*;Blank;*;Nums;*;Blank;*;Date;*] *)
            ([0;2], RCell(RToken(NilToken)));             (* [' ';Nums;Blank;*;Nums;*;Blank;*;Date;*] *)
            ([0;5], RCell(RToken(NilToken)));             (* [' ';Nums;Blank;*;Nums;Blank;*;Date;*] *)
            ([0;6], RCell(RToken(NilToken)));             (* [' ';Nums;Blank;*;Nums;Blank;Date;*] *)
            ([0;3], RCell(RToken(NilToken)));             (* [' ';Nums;Blank;Nums;Blank;Date;*] *)
            ([0;6], RCell(RToken(NilToken)));             (* [' ';Nums;Blank;Nums;Blank;Date] *)
            ([0;1], RCell(RMerge));                       (* [' ';List(Nums);Blank;Date] *)
            ([1;0], RCell(RToken(RegexToken(Nums))));     (* [*;Nums;*] *)
            ([1;2], RCell(RToken(RegexToken(Nums))));     (* [*;Nums;*;Nums;*])*)
            ([1;2], RCell(RToken(RegexToken(Blank))));    (* [*;Nums;*;Blank;*;Nums;*] *)
            ([1;2], RCell(RToken(NilToken)));             (* [*;Nums;Blank;*;Nums;*] *)
            ([1;3], RCell(RToken(RegexToken(Blank))));    (* [*;Nums;Blank;*;Blank;*;Nums;*] *)
            ([1;0], RCell(RToken(NilToken)));             (* [Nums;Blank;*;Blank;*;Nums;*] *)
            ([1;6], RCell(RToken(NilToken)));             (* [Nums;Blank;*;Blank;*;Nums] *)
            ([1;2], RCell(RToken(RegexToken(Nums))));     (* [Nums;Blank;*;Nums;*;Blank;*;Nums] *)
            ([1;2], RCell(RToken(NilToken)));             (* [Nums;Blank;Nums;*;Blank;*;Nums] *)
            ([1;3], RCell(RToken(NilToken)));             (* [Nums;Blank;Nums;Blank;*;Nums] *)
            ([1;4], RCell(RToken(NilToken)));             (* [Nums;Blank;Nums;Blank;Nums] *)
            ([1;3], RCell(RToken(NilToken)));             (* [Nums;Blank;Nums;Blank;Nums] *)
            ([1;0], RCell(RMerge));                       (* [List(Nums);Blank;Nums] *)
            ([1;0], RCell(RMerge));                       (* [List(Nums)] *)
           ] in
  
  complete_example [[AnyToken];[AnyToken]] rl ls
                                                                  