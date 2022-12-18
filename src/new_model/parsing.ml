open Types_utilities

(* regexp definitions *)

type ('a, 'b) parseur = 'a -> 'b Myseq.t
exception Parse_failure

let re_str_alphas = "[A-Za-z_0-9]+"
let re_str_nums = "[0-9]+"
let re_str_letters = "[A-Za-z]+"
let re_str_date = {|[0-3]?[0-9]\(/\|-\|\.\)\(\([0-1]?[0-9]\)\|\(jan\)\|\(fev\)\|\(mar\)\|\(avr\)\|\(jun\)\|\(jul\)\|\(aou\)\|\(oct\)\|\(nov\)\|\(dec\)\)\(/\|-\|\.\)-?[0-9]?[0-9]?[0-9]?[0-9]|}
let re_str_blank = "[ \t]+"

let re_alphas = Str.regexp re_str_alphas
let re_letters = Str.regexp re_str_letters
let re_nums = Str.regexp re_str_nums
let re_date = Str.regexp re_str_date
let re_blank = Str.regexp re_str_blank

let re_of_regex_token = function
  | Alphas -> re_alphas
  | Nums -> re_nums
  | Letters -> re_letters
  | Date -> re_date
  | Blank -> re_blank

(* a split of a string s on k is the pair (s[0:k], s[k:n]) where n is the length of s *)
type split = string * string

let regex_match_full re s = 
  (Str.string_match re s 0) && (Str.match_end () = String.length s)

let is_blank_token (m : token_model) =
  match m with
  | RegexToken(Blank) -> true
  | ConstToken s -> regex_match_full re_blank s
  | _ -> false
  
(* generate all splits of a string s ie (s[0:0], s[0:n]), (s[0:1], s[1:n]), ... ,(s[0:n], s[n:n]) *)
let all_string_splits (s : string) : split Myseq.t =
  Printf.printf "Parsing.all_string_splits\n";
  if s = "" then Myseq.return ("", "") else
  let n = String.length s in
  let rec aux k =
    if k = n then
      Myseq.return (s, "")
    else
      Myseq.cons (String.sub s 0 k, String.sub s k (n-k)) (aux (k+1))
  in Myseq.cons ("", s) (aux 1)

(* generate all split (a,b) of the string s such that the first matches of the regexp re on the begining of the string s*)
let all_regex_matches (re : Str.regexp) (s : string) : split Myseq.t =
  Printf.printf "Parsing.all_regex_matches\n";
  if s = "" then Myseq.empty else
  let n = String.length s in
  if regex_match_full re s then
      Myseq.return (s, "")
  else (
    if Str.string_match re s 0 then
      let k = Str.match_end () in
      let sub_s = String.sub s k (n-k) in
      Myseq.return (Str.matched_string s, sub_s)
    else
      Myseq.empty)
      

let regex_token_parse (re : Str.regexp) (s : string) : (string * token_data * string) Myseq.t =
  Printf.printf "Parsing.regex_token_parse\n";
  assert (s <> "");
  let len = String.length s in
  (* beware of the re matches "" *)
  let rec aux start =
    if start >= len
    then Myseq.empty
    else
      try
        let i = Str.search_forward re s start in
        let j = Str.match_end () in
        if i = j (* not keeping nil matches *)
        then aux (i+1)
        else Myseq.cons (i,j) (aux j)
      with Not_found ->
        Myseq.empty 
  in
  let* i, j = aux 0 in
  let sl = String.sub s 0 i in
  let st = String.sub s i (j-i) in
  let sr = String.sub s j (len-j) in
  let dt = DString st in
  Myseq.return (sl,dt,sr)

let list_token_parse (m : token_model) (s : string) : string * string * string =
  Printf.printf "Parsing.list_token_parse\n";
  if s = "" then ("", "", "") else
  let n = String.length s in
  let re =
    match m with 
    | RegexToken re -> re_of_regex_token re
    | ConstToken s -> Str.regexp_string s
    | _ -> assert false in
  if Str.string_match re s 0 then (
    let item = Str.matched_string s in
    let s = String.sub s (Str.match_end ()) (n-Str.match_end ()) in
    let n = String.length s in
    if Str.string_match (re_blank) s 0 then (
      let sep = Str.matched_string s in
      (item, sep, String.sub s (Str.match_end ()) (n-Str.match_end ())) )
    else
      (item, "", s) )
  else
    ("", "", s)

(* generate, from the string s, all possible token_data lists of token_model *)
let all_list_splits (m : token_model) (s : string) : (token_data list * string) Myseq.t =
  Printf.printf "Parsing.all_list_splits\n";
  if s = "" then Myseq.empty else
  let rec aux acc s = 
    let t, sep, sr = list_token_parse m s in
    if t = "" then (
      Myseq.empty)
    else (
      let l = acc@[DString t] in
      if sep = "" then(
        Myseq.return (l, sr))
      else
        Myseq.cons (l, sep^sr) (aux (l@[DString sep]) sr) )
  in aux [] s


(* parsing function *)
let cell_parse (m : cell_model) (s : string) : cell_data Myseq.t =
  Printf.printf "Parsing.cell_parse\n";
  let rec aux_parse (m : cell_model) (s :string) : cell_data Myseq.t =
    match m with
    | [] -> 
        if s = "" then
          Myseq.return ([])
        else 
          Myseq.empty
    | NilToken :: q -> 
        let* l = (aux_parse q s) in 
        Myseq.return (DNil::l)
    | AnyToken :: q -> 
        let* sl, sr = all_string_splits s in
        let* l = (aux_parse q sr) in 
        Myseq.return ((DAny sl)::l)
    | (ConstToken c) :: q -> 
        let* sl, sr = all_regex_matches (Str.regexp_string c) s in
        let* l = (aux_parse q sr) in
        Myseq.return ((DString sl)::l)
    | (RegexToken re) :: q ->
        let* sl, sr = all_regex_matches (re_of_regex_token re) s in
        let* l = (aux_parse q sr) in
        Myseq.return ((DString sl)::l)
    | (ListToken m) :: q ->
        let* slist, sr = all_list_splits m s in
        let* l = (aux_parse q sr) in
        Myseq.return ((DList slist)::l)
    | _ -> assert false
  in aux_parse m s

let row_parse (lm : row_model) : (string list, row_data) parseur =
  Printf.printf "Parsing.row_parse\n";
  fun ls -> Myseq.product_fair (List.map2 cell_parse lm ls)
