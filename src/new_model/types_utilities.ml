
(* parameters *)

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v

let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" 256 string_of_int (* max nb of considered doc parses *)
let max_nb_reads = def_param "max_nb_doc_reads" 3 string_of_int (* max nb of selected doc reads, passed to the next stage *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_refinements = def_param "max_refinements" 100 string_of_int (* max nb of considered refinements *)


(* utilities *)
module List = struct
  include List
  let rec from_nth l n =
    match l, n with
    | [], _ -> []
    | _, 0 -> l
    | _::q, _ -> from_nth q (n-1)
  let rec map_from f l n =
    match l, n with
    | [], _ -> []
    | t::q, 0 -> let r = f t in r::(map_from f q 0)
    | t::q, _ -> t::(map_from f q (n-1))
  let rec cut_at_n l n =
    let rec aux l n = 
      match l, n with
      | [], _ -> [], []
      | _, 0 -> [], l
      | t::q, _ -> let front, queue = aux q (n-1) in t::front, queue
    in aux l n
end
(* open Task *)

module Seq = Stdlib.Seq

exception TODO

module TEST = (* for profiling visually, used for the JS version *)
  struct
    let prof name f =
      print_endline (name ^ "...");
      let res = f () in
      print_endline ("DONE " ^ name);
      res
  end

let rec list_update (f : 'a -> 'a) (i : int) : 'a list -> 'a list = function
  | [] -> raise Not_found
  | x::l ->
     if i = 0
     then f x :: l
     else x :: list_update f (i-1) l
  
type 'a result = ('a,exn) Result.t
let ( let| ) res f = Result.bind res f [@@inline]

let rec list_map_result (f : 'a -> ('b,'c) Result.t) (lx : 'a list) : ('b list, 'c) Result.t =
  match lx with
  | [] -> Result.Ok []
  | x::lx1 ->
     let| y = f x in
     let| ly1 = list_map_result f lx1 in
     Result.Ok (y::ly1)

let result_list_bind_some (lx_res : ('a list,'c) Result.t) (f : 'a -> ('b list,'c) Result.t) : ('b list, 'c) Result.t =
  let rec aux = function
  | [] -> invalid_arg "Model2.bind_map_ok: empty list"
  | [x] -> f x
  | x::lx1 ->
     let open Result in
     match f x, aux lx1 with
     | Ok ly0, Ok ly1 -> Ok (List.append ly0 ly1)
     | Ok ly0, Error _ -> Ok ly0
     | Error _, Ok ly1 -> Ok ly1
     | Error e1, Error _ -> Error e1
  in
  let| lx = lx_res in
  aux lx
let ( let+|+ ) = result_list_bind_some

let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]
let ( let*? ) seq f = seq |> Myseq.filter_map f [@@inline]
let ( let*! ) seq f = seq |> Myseq.map f [@@inline]
let seq_cons_if cond x seq =
  if cond
  then Myseq.cons x seq
  else seq
let seq_concat_if cond seq1 seq2 =
  if cond
  then Myseq.concat [seq1; seq2]
  else seq2

type 'a triple = 'a * 'a * 'a

type dl = Mdl.bits

let dl0 = 0.

let dl_round dl = Float.round (dl *. 1e9) /. 1e9

let dl_compare (dl1 : float) (dl2 : float) =
  if dl1 < dl2 then -1
  else if dl1 = dl2 then 0
  else 1 [@@inline]

type var = int list
type expr = var Expr.expr

type row_model = cell_model list
and cell_model = token_model list
and token_model =
  | NilToken
  | AnyToken
  | RegexToken of regex_model
  | ConstToken of string
  | ListToken of token_model
  | ExprToken of expr
and regex_model =
  | Alphas
  | Letters
  | Nums
  | Date
  | Blank

type row_data = cell_data list
and cell_data = token_data list
and token_data =
  | DNil
  | DAny of string
  | DList of cell_data
  | DString of string

type env = row_data

(* reading *)
type 'a read = env * 'a * dl
type row_read = env * row_data * dl
type cell_read = env * cell_data * dl
type token_read = env * token_data * dl

(* refinements *)
type token_refinement =
  | RToken of token_model (* token specialization *)
  | RMerge
type cell_refinement = 
  RCell of token_refinement
type refinement =
  | RInit
  | Rinput of var * cell_refinement * dl (* estimated result DL *)
  | Routput of var * cell_refinement * dl (* estimated result DL *)

(* examples  / pairs *)       
type model =
  { input_model : row_model; (* no reference *)
    output_model : row_model
  }

type pairs_reads = (* result of reading a list of pairs of grids *)
{ dl_mi : dl; (* input model DL *)
  dl_mo : dl; (* output model DL *)
  inputs_reads : row_read list list; (* outer list over example inputs, inner list over parses *)
  reads : (row_read * row_read * dl) list list; (* outer list over examples, inner list over parses, sorted in increasing DL *)
}

