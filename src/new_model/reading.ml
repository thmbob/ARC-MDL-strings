open Types_utilities
open Parsing
open Environment
open Dl_compute
open Printing

(* reading *)

let limit_dl (f_dl : 'a -> dl) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x0::_ ->
     let dl0 = f_dl x0 in
     let min_dl = !max_parse_dl_factor *. dl0 in
     List.filter (fun x -> f_dl x <= min_dl) l

let read_row ~(env : env) (m0 : row_model) (ls : string list) : row_read list result =
  Printf.printf "Reading.read_row m:"; print_row_model m0; Printf.printf " env:"; print_row_data env; Printf.printf " ls:"; print_list print_string ls; Printf.printf "\n"; 
  Common.prof "Model.read_row" (fun () ->
  let| m = row_apply m0 env in (* reducing expressions *)
  Printf.printf "Reading.read_row (reduced expressions) m:"; print_row_model m0;
  let parses =
    let* data = row_parse m ls in
    let dl = (* QUICK *)
      let dl_data = row_encoder m data in
      (* rounding before sorting to absorb float error accumulation *)
      dl_round dl_data in
    Myseq.return (env, data, dl) in
  let l_parses =
    Common.prof "Model.read_doc/first_parses" (fun () ->
        parses
        |> Myseq.slice ~offset:0 ~limit:(!max_nb_parse)
        |> Myseq.to_list) in
  if l_parses = []
  then Result.Error Parse_failure
  else
    let best_parses =
      l_parses (* QUICK *)
      |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
      |> (fun l -> Common.sub_list l 0 !max_nb_reads)
      |> limit_dl (fun (_,_,dl) -> dl)
      |> List.mapi (fun rank (env,data,dl) ->
             let dl = dl +. Mdl.Code.universal_int_star rank in (* to penalize later parses, in case of equivalent parses *)
             (env, data, dl)) in
    List.iter (fun (_, data, dl) -> Printf.printf "best_parse:\n\tdata:"; print_row_data data; Printf.printf "\tdl:%f\n" dl) best_parses;
    Result.Ok best_parses)

type rows_reads =
{ dl_m : dl; (* DL of the model *)
  reads : row_read list list; (* outer list over docs, inner list over parses, sorted in increasing DL *)
}