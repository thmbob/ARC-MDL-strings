open Types_utilities

let dl_round dl = Float.round (dl *. 1e9) /. 1e9

let dl_compare (dl1 : float) (dl2 : float) =
  if dl1 < dl2 then -1
  else if dl1 = dl2 then 0
  else 1 [@@inline]

(* ASCII chars *)

let ascii_init_occs =
  (* freely adapted from http://millikeys.sourceforge.net/freqanalysis.html *)
  [ ' ', 70.;
    'E', 18.;
    'e', 18.;
    'T', 13.;
    't', 13.;
    'A', 12.;
    'a', 12.;
    'O', 11.;
    'o', 11.;
    'I', 10.;
    'i', 10.;
    'N', 10.;
    'n', 10.;
    '0', 10.;
    '1', 10.;
    '2', 10.;
    '3', 10.;
    '4', 10.;
    '5', 10.;
    '6', 10.;
    '7', 10.;
    '8', 10.;
    '9', 10.;
    'H', 9.;
    'h', 9.;
    'S', 9.;
    's', 9.;
    'R', 8.;
    'r', 8.;
    'D', 7.;
    'd', 7.;
    'L', 6.;
    'l', 6.;
    'U', 4.;
    'u', 4.;
    'M', 4.;
    'm', 4.;
    'C', 4.;
    'c', 4.;
    'W', 3.5;
    'w', 3.5;
    'G', 3.;
    'g', 3.;
    'F', 3.;
    'f', 3.;
    'Y', 3.;
    'y', 3.;
    'P', 2.5;
    'p', 2.5;
    '_', 5.;
    ',', 4.8;
    '.', 4.7;
    'B', 2.3;
    'b', 2.3;
    'K', 1.4;
    'k', 1.4;
    'V', 1.4;
    'v', 1.4;
    '"', 2.6;
    '\'', 1.7;
    '-', 1.0;
    '?', 0.47;
    'X', 0.23;
    'x', 0.23;
    'J', 0.22;
    'j', 0.22;
    ';', 0.31;
    '!', 0.30;
    'Q', 0.14;
    'q', 0.14;
    'Z', 0.13;
    'z', 0.13;
  ]
       
let ascii_chars =
  let l = ref [] in
  for i = 0 to 127 do
    l := Char.chr i :: !l
  done;
  !l
let make_ascii_prequential = new Mdl.Code.prequential ascii_chars (* no assumption on char freq, not to spoil the value of specific regexs *)

let letter_chars =
  let l = ref [] in
  for i = Char.code 'a' to Char.code 'z' do l := Char.chr i :: !l done;
  for i = Char.code 'A' to Char.code 'Z' do l := Char.chr i :: !l done;
  !l
let make_letter_prequential = new Mdl.Code.prequential ~init_occs:ascii_init_occs letter_chars

let num_chars =
  let l = ref [] in
  for i = Char.code '0' to Char.code '9' do l := Char.chr i :: !l done;
  !l
let make_num_prequential = new Mdl.Code.prequential num_chars

let alpha_chars = '_' :: num_chars @ letter_chars
let make_alpha_prequential = new Mdl.Code.prequential ~init_occs:ascii_init_occs alpha_chars

let symbol_chars = ['\'';'/';'-';'!';'.';';';',';'?';'+';'#';'_']
let make_symbol_prequential = new Mdl.Code.prequential ~init_occs:ascii_init_occs symbol_chars

let uppercases_chars = let l = ref [] in for i = Char.code 'A' to Char.code 'Z' do l := Char.chr i :: !l done; !l
let make_uppercase_prequential = new Mdl.Code.prequential ~init_occs:ascii_init_occs uppercases_chars

let lowercases_chars = let l = ref [] in for i = Char.code 'a' to Char.code 'z' do l := Char.chr i :: !l done; !l
let make_lowercase_prequential = new Mdl.Code.prequential ~init_occs:ascii_init_occs lowercases_chars

let blank_symbols = ['\t';' ']
let make_blank_prequential = new Mdl.Code.prequential blank_symbols
     
let dl_char_plus (make_pc : unit -> char Mdl.Code.prequential) (s : string) : dl =
  (* using prequential code *)
  let pc = make_pc () in
  String.iter
    (fun c -> ignore (pc#code c))
    s;
  pc#cumulated_dl

(* models' descriptive lengths *)
  
let rec dl_row_model ~(env : row_model) (lm : row_model) : dl =
  let nb_env_paths = dl_row_model_env_stats env in (* useful view on the environment model *)
  Mdl.Code.universal_int_plus (List.length lm)
  +. List.fold_left (fun acc cm -> acc +. dl_cell_model ~nb_env_paths cm) 0. lm
and dl_cell_model ~nb_env_paths (cm : cell_model) : dl =
  Mdl.Code.universal_int_plus (List.length cm)
  +. List.fold_left (fun acc tm -> acc +. dl_token_model ~nb_env_paths tm) 0. cm
and dl_row_model_env_stats (lm : row_model) : int =
  List.fold_left (fun res cm -> res + dl_cell_model_env_stats cm) 0 lm
and dl_cell_model_env_stats (cm : cell_model) : int =
  List.fold_left (fun res tm -> res + dl_token_model_env_stats tm) 0 cm
and dl_token_model_env_stats : token_model -> int = function
  (* counting paths to tokens *)
  | NilToken -> 0
  | AnyToken -> 0
  | _ -> 1
and dl_token_model ~nb_env_paths (tm : token_model) : dl =
  match tm with
  | NilToken -> Mdl.Code.usage 0.2
  | AnyToken -> Mdl.Code.usage 0.02
  | ConstToken s ->
     Mdl.Code.usage 0.1
     +. Mdl.Code.universal_int_plus (String.length s)
     +. dl_char_plus make_ascii_prequential s
  | RegexToken re ->
     Mdl.Code.usage 0.2
     +. dl_regex_model re
  | ExprToken e ->
     Mdl.Code.usage 0.38
     +. Expr.dl_expr (fun p -> Mdl.Code.uniform nb_env_paths) e
  | ListToken c ->
     Mdl.Code.usage 0.1
     +. dl_token_model ~nb_env_paths c
and dl_regex_model : regex_model -> dl = function
  | Alphas -> Mdl.Code.usage 0.525
  | Nums -> Mdl.Code.usage 0.15
  | Letters -> Mdl.Code.usage 0.025
  | Date -> Mdl.Code.usage 0.1
  | Blank -> Mdl.Code.usage 0.1

(* encoders : descriptive lengths of datas *)

type 'a encoder = 'a -> dl

let rec row_data_length (ld : row_data) : int =
  List.fold_left (fun len cd -> len + (cell_data_length cd)) 0 ld
and cell_data_length (cd : cell_data) : int =
  List.fold_left (fun len td -> len + (token_data_length td)) 0 cd
and token_data_length = function
  | DNil -> 0
  | DAny s -> String.length s
  | DList l -> cell_data_length l
  | DString s -> String.length s

let rec row_encoder (lm : row_model) : row_data encoder =
  (* assuming that the passed data matches the model *)
  let l_enc_cm = List.map cell_encoder lm in
  fun ld -> List.fold_left2 (fun res enc_cm d -> res +. enc_cm d) 0. l_enc_cm ld
and cell_encoder (cm : cell_model) : cell_data encoder =
  let l_enc_tm = List.map token_encoder cm in
  fun cd -> List.fold_left2 (fun res enc_m d -> res +. enc_m d) 0. l_enc_tm cd
and token_encoder (m : token_model) : token_data encoder =
  let enc_m = token_encoder_aux m in
  fun d ->
    let n = token_data_length d in
    Mdl.Code.universal_int_star n
    +. enc_m d
and token_encoder_aux : token_model -> token_data encoder = function
  | NilToken ->
     (function
      | DNil -> 0.
      | _ -> assert false)
  | AnyToken ->
     (function
      | DAny s -> dl_char_plus make_ascii_prequential s
      | _ -> assert false)
  | ConstToken _ -> 
     (function 
      | DString _ -> 0.
      | _ -> assert false)
  | RegexToken re ->
     let enc_re = regex_encoder re in
     (function 
       | DString s -> enc_re s
       | _ -> assert false)
  | ExprToken _ -> 
     (fun _ -> 0.) (* nothing to code, evaluated *)
  | ListToken m -> 
     (function 
      | DList ld -> 
          let enc_m = token_encoder m in
          let enc_blank = token_encoder_aux (RegexToken Blank) in
          let rec aux_list ld =
            match ld with
            | [] -> 0.
            | t::[] -> enc_m t
            | t::sep::q -> enc_m t +. enc_blank sep +. aux_list q in
          Mdl.Code.universal_int_star (List.length ld)
          +. aux_list ld
      | _ -> assert false)
and regex_encoder : regex_model -> string encoder = function
  | Alphas -> dl_char_plus make_alpha_prequential
  | Nums -> dl_char_plus make_num_prequential
  | Letters -> dl_char_plus make_letter_prequential
  | Date -> (fun s -> Mdl.log2 20000.0 +. Mdl.log2 366.0)
  | Blank -> dl_char_plus make_blank_prequential