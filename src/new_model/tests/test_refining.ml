open Types_utilities
open Parsing
open Printing
open Environment
open Dl_compute
open Writing
open Reading
open Refinement

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
  let parse_seq = row_parse m s in
  Printf.printf "\nParsing result :\n";
  Myseq.iter print_row_data parse_seq;
  let var_seq = Myseq.map row_bindings parse_seq in
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
  Myseq.iter (fun rd -> let dl_d = enc_m rd in Printf.printf "m:%f d:%f sum:%f" dl_m dl_d (dl_m+.dl_d)) parse_seq;
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

let generate_rsr (m : row_model) (l : string list list) : rows_reads =
  let rec aux acc l =
    match l with 
    | [] -> acc
    | t::q -> 
        let r = (read_row ~env:[] m t) in
        let l = 
          (match r with
          | Result.Ok l -> l
          | Result.Error _ -> assert false ) in
        aux (l::acc) q
  in
  { dl_m = (dl_row_model ~env:m m) ; reads = aux [] l}

let rec compute_refinements (l : string list list) old_model m =
  if old_model <> m then
    let rsr = generate_rsr m l in
    let seq = row_refinements ~nb_env_paths:0 m rsr in
    Myseq.iter
      (fun (v, cr, dl, m) -> 
        Printf.printf "\nNew refinement : \n";
        Printf.printf "\tv:\n\t\t";
        print_var v;
        Printf.printf "\n\tr:\n\t\t";
        print_cell_refinement cr;
        Printf.printf "\n\tdl:\n\t\t";
        print_float dl;
        Printf.printf "\n\tnew model:\n\t\t";
        print_row_model m; print_newline ()
        )
    (Myseq.sort (fun (_,_,dl1,_) (_,_,dl2,_) -> dl_compare dl1 dl2) seq);
    match Myseq.uncons seq with 
      | None -> Printf.printf "\nFinal model :\n"; print_row_model m; Printf.printf "\nend of program\n"
      | Some ((v, tr, dl, new_model), _) -> compute_refinements l m new_model   
  else
    ()

let () =
  let sl = [["Karim Benzema - 19/dec/1987"]; ["Antoine Griezman - 21/3/1991"]; ["Eduardo Camavinga - 10/11/2002"]] in
  compute_refinements sl [] [[AnyToken]]