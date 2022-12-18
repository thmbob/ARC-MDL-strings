(* utilities *)
          
(* other modules *)
open Types_utilities
open Printing
open Parsing
open Dl_compute
open Environment
open Writing
open Reading

(* refinements *)
let map_reads (f : 'a -> 'b) (reads : 'a list list) : 'b list list  =
  List.map
    (fun example_reads ->
      List.map f example_reads)
    reads

let inter_union_reads
      (f : 'read -> ('r * 'd) list)
      (reads : 'read list list)
    : ('r, ('read * 'd) list) Mymap.t =
    Printf.printf "Refinement.inter_union_reads\n";
  (* given a function extracting refinement information [type 'r] from each read,
     return a set of such ref-info, each mapped to the dl-shortest reads supporting it, along with new data *)
  let process_example reads =
    List.fold_left
      (fun res read ->
        List.fold_left
          (fun res (r,data') ->
            if Mymap.mem r res
            then res
            else Mymap.add r (read,data') res)
          res (f read))
      Mymap.empty reads in
  match reads with
  | [] -> assert false
  | example0_reads :: other_reads ->
     let res0 =
       process_example example0_reads
       |> Mymap.map (fun best_read -> [best_read]) in
     List.fold_left
       (fun res exampleI_reads ->
         let resI = process_example exampleI_reads in
         Mymap.merge
           (fun r best_reads_opt best_readI_opt ->
             match best_reads_opt, best_readI_opt with
             | Some best_reads, Some best_readI -> Some (best_readI::best_reads)
             | _ -> None)
           res resI)
       res0 other_reads

let row_refinements ~nb_env_paths (lm : row_model) ?(dl_M : dl = 0.) (rsr : rows_reads) : (var * int * cell_refinement * dl * row_model) Myseq.t =
  (* NOTE: dl_M does not matter for ranking because invariant of parsing and refinement *)
  Printf.printf "Refinement.row_refinements:"; print_row_model lm; print_newline ();
  let reads = (* replacing env's with expression index's over them *)
    map_reads
      (fun (env,data,dl) ->
        Printf.printf "Refinement.row_refinements.lambda_in_map_reads : env:"; print_row_data env; Printf.printf "data: "; print_row_data data; Printf.printf "dl:%f\n" dl;
        ((let i = Expr.make_index (row_bindings env) in print_index i; i), data, dl))
      rsr.reads in
  let rec fold_row lm reads =
    Printf.printf "Refinement.row_refinements.fold_row\n";
    Myseq.interleave
      (List.mapi
         (fun i m ->
           let m_reads =
             map_reads
               (fun (env, ld, dl) ->
                 let d_i = try List.nth ld i with _ -> assert false in
                 (env, d_i, dl))
               reads in
            Myseq.map (fun (v, r, dl) -> (i::v, r, dl)) (fold_cell m m_reads))
         lm)
  and fold_cell (cm : cell_model) reads : (var * cell_refinement * dl) Myseq.t =
  Printf.printf "Refinement.row_refinements.fold_cell\n";
    let rec merge_search cm v = 
      Printf.printf "Refinement.row_refinements.merge_search v:"; print_int v; print_newline ();
      let _::next_cm = cm in
      match cm with
      | [] | [_] | [_;_] -> Myseq.empty
      | ListToken(tm)::blank::ListToken(tm2)::q when (tm=tm2 && is_blank_token(blank)) -> 
          Printf.printf "Merge attempt: "; print_token_model (ListToken(tm)); print_token_model blank; print_token_model (ListToken(tm2)); print_newline ();
          Myseq.cons ([v], RCell RMerge, 0.01) (merge_search next_cm (v+1))
      | ListToken(tm)::blank::t::q when (t=tm && is_blank_token(blank)) -> 
          Printf.printf "Merge attempt: "; print_token_model (ListToken(tm)); print_token_model blank; print_token_model t; print_newline ();
          Myseq.cons ([v], RCell RMerge, 0.01) (merge_search next_cm (v+1))
      | tm::blank::ListToken(t)::q when (t=tm && is_blank_token(blank)) -> 
          Printf.printf "Merge attempt: "; print_token_model tm; print_token_model blank; print_token_model (ListToken(t)); print_newline ();
          Myseq.cons ([v], RCell RMerge, 0.01) (merge_search next_cm (v+1))
      | tm::blank::ConstToken(s2)::q when (tm=ConstToken(s2) && is_blank_token(blank)) -> 
          Printf.printf "Merge attempt: "; print_token_model tm; print_token_model blank; print_token_model (ConstToken(s2)); print_newline ();
          Myseq.cons ([v], RCell RMerge, 0.01) (merge_search next_cm (v+1))
      | tm::blank::RegexToken(re2)::q when (tm=RegexToken(re2) && is_blank_token(blank)) -> 
          Printf.printf "Merge attempt: "; print_token_model tm; print_token_model blank; print_token_model (RegexToken(re2)); print_newline ();
          Myseq.cons ([v], RCell RMerge, 0.01) (merge_search next_cm (v+1)) 
      | _::q -> merge_search q (v+1) 
    in
    Myseq.interleave
      ((merge_search cm 0)::
        (List.mapi
         (fun i m ->
           let m_reads =
             map_reads
               (fun (env, ld, dl) ->
                 let d_i = try List.nth ld i with _ -> assert false in
                 (env, d_i, dl))
               reads in
            Myseq.map (fun (r, dl) -> ([i], RCell r, dl)) (fold_token m m_reads))
         cm))
  and fold_token (m : token_model) reads : (token_refinement * dl) Myseq.t  =
  Printf.printf "Refinement.row_refinements.fold_token\n";
  match m with
    | NilToken -> Myseq.empty
    | AnyToken ->
        Printf.printf "Any refinements\n";
        let encoder_m = token_encoder m in
        let dl_m = dl_token_model ~nb_env_paths m in
        (* researc of the  possible refinements *)
        let r_best_reads : ([`IsNil | `RE of regex_model | `CommonStr of string ], _) Mymap.t =
          let compare_regex_slices (best_len, best_slice as best : int * (string * token_data * string)) (_, data', _ as slice : (string * token_data *string)) =
            let len = token_data_length data' in
            if len > best_len then 
              (len, slice)
            else 
              best 
          in
          let extract_best_regex_slice (s :string) rs (re : regex_model) =
            let best_len, (sl, data', sr as best_slice) =
              Myseq.fold_left compare_regex_slices (0, ("", DString "", "")) (regex_token_parse (re_of_regex_token re) s) in
            if best_len > 0 then 
              (`RE re, (DAny sl, data', DAny sr)) :: rs
            else 
              rs
          in
          let possible_refinements read =
            let _, data, _ = read in
            let s = token_of_token_data data in
            let rs = (* Nil refinement *)
              if s = "" then 
                [(`IsNil, (DNil, DNil, DNil))] 
              else 
                [] in
            let rs = (* Regex refinements *)
              if s <> "" then
                List.fold_left (extract_best_regex_slice s) rs [Alphas; Letters; Nums; Date]
              else rs in
            let rs = (* Const refinement *)
              if s <> "" then 
                (`CommonStr s, (DNil, DString s, DNil)) :: rs
              else rs in
            rs
          in
          inter_union_reads possible_refinements reads 
        in
        (* estimation of the description length of the refinement*)
        let* r_info, best_reads = Mymap.to_seq r_best_reads in
        let m' =
          match r_info with
          | `IsNil -> NilToken
          | `RE re -> RegexToken re
          | `CommonStr s -> ConstToken s in
        let r = RToken m' in
        let encoder_m' = token_encoder m' in
        let encode_l_r = token_encoder (match m' with
                                        | RegexToken _ -> AnyToken
                                        | _ -> NilToken) in
        let dl_m' = dl_token_model ~nb_env_paths m' in
        let dl' =
          dl_M -. dl_m +. dl_m'
          +. !alpha *. Mdl.sum best_reads
                         (fun ((_,data,dl_D), (data_l, data', data_r)) ->
                           dl_D -. encoder_m data +. encoder_m' data' +. encode_l_r data_l +. encode_l_r data_r) in
        Myseq.return (r, dl')
    | ConstToken s -> Myseq.empty
    | RegexToken re ->
        Printf.printf "Regex refinements\n";
        let encoder_m = token_encoder m in
        let dl_m = dl_token_model ~nb_env_paths m in
        let re'_candidates =
          match re with
          | Alphas -> [Nums; Letters]
          | _ -> [] in
        let r_best_reads : ([`CommonStr of string | `RE of regex_model | `Expr of expr], _) Mymap.t =
          let refine_with_regexp s rs re' =
            if regex_match_full (re_of_regex_token re') s then 
              (`RE re', DString s) :: rs
            else 
              rs
          in
          let refine_with_expr s rs e =
            (`Expr e, DString s) :: rs
          in
          let possible_refinements read =
            let idx, data, _ = read in
            let s = token_of_token_data data in
            let es = Expr.index_lookup (`String s) idx in
            let rs = (* Const refinement *)
                if s <> "" then 
                  [(`CommonStr s, DString s)] 
                else [] in
            let rs = (* Regex refinements (full match only) *)
              List.fold_left (refine_with_regexp s) rs re'_candidates in
            let rs = (* Expression refinements *)
              Myseq.fold_left (refine_with_expr s) rs (Expr.exprset_to_seq es) in
            rs
          in
          inter_union_reads possible_refinements reads 
        in
        let* r_info, best_reads = Mymap.to_seq r_best_reads in
        let m' =
          match r_info with
          | `CommonStr s -> ConstToken s
          | `RE re' -> RegexToken re'
          | `Expr e -> ExprToken e in
        let r = RToken m' in
        let encoder_m' = token_encoder m' in
        let dl_m' = dl_token_model ~nb_env_paths m' in
        let dl' =
          dl_M -. dl_m +. dl_m'
          +. !alpha *. Mdl.sum best_reads
                         (fun ((_,data,dl_D), data') ->
                           dl_D -. encoder_m data +. encoder_m' data') in         
        Myseq.return (r, dl')
    | ListToken cm ->
        Printf.printf "List refinements\n"; 
        let encoder_m = token_encoder m in
        let dl_m = dl_token_model ~nb_env_paths m in
        let r_best_reads : ([`CommonStr of string | `RE of regex_model | `Expr of expr], _) Mymap.t =
          let refine_with_expr l rs e =
            (`Expr e, DList l) :: rs
          in
          let possible_refinements read =
            let idx, data, _ = read in
            let DList l = data in
            let l' = Expr.get_even_elements (fun x -> `String(token_of_token_data x)) l in 
            let es = Expr.index_lookup (`List l') idx in
            Printf.printf "Refinements.row_refinement.token_refinement.list_refinement.possible_refinements es:"; 
            print_exprset print_var es; print_newline ();
            let rs = (* Expression refinements *)
              Myseq.fold_left (refine_with_expr l) [] (Expr.exprset_to_seq es) in
            rs
          in
          inter_union_reads possible_refinements reads 
        in
        let* r_info, best_reads = Mymap.to_seq r_best_reads in
        let m' =
          match r_info with
          | `CommonStr s -> ConstToken s
          | `RE re' -> RegexToken re'
          | `Expr e -> ExprToken e in
        let r = RToken m' in
        let encoder_m' = token_encoder m' in
        let dl_m' = dl_token_model ~nb_env_paths m' in
        let dl' =
          dl_M -. dl_m +. dl_m'
          +. !alpha *. Mdl.sum best_reads
                         (fun ((_,data,dl_D), data') ->
                           dl_D -. encoder_m data +. encoder_m' data') in         
        Myseq.return (r, dl')
    | ExprToken e -> Myseq.empty
  in
  let* p, r, dl' =
    fold_row lm reads
    |> Myseq.sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
    |> Myseq.slice ~limit:!max_refinements in
  let m', shift = apply_cell_refinement r p lm in
  Myseq.return (p, shift, r, dl', m')

