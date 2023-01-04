(* Managing expressions for expressing computations in models *)

(* utilities *)

type 'a result = ('a,exn) Result.t
let ( let| ) res f = Result.bind res f [@@inline]

let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]

type dl = Mdl.bits

(* types *)
        
type typ = [`Gen | `String | `Int | `Date | `Time | `List | `Function ]
     
(* values *)

let rec get_even_elements f l =
  let rec aux l idx =
    match l, idx with
    | [], _ -> []
    | t::q, i when i mod 2 = 0 -> (f t)::(aux q (idx+1))
    | t::q, _ -> (aux q (idx+1)) in
  aux l 0
         
type value =
  [ 
  | `String of string
  | `Int of int 
  | `Date of int * int * int 
  | `Time of int * int * int
  | `List of value list
  | `Function of (value -> value result)
  ]

let string_of_value : value -> string result = function
  | `String s -> Result.Ok s
  | _ -> Result.Error (Invalid_argument "Expr.string_of_value") (* an ill-formed expression was built *)
let int_of_value : value -> int result = function
  | `Int i -> Result.Ok i
  | _ -> Result.Error (Invalid_argument "Expr.int_of_value")
let date_of_value : value -> (int*int*int) result = function
  | `Date d -> Result.Ok d
  | _ -> Result.Error (Invalid_argument "Expr.date_of_value")
let time_of_value : value -> (int*int*int) result = function
  | `Time t -> Result.Ok t
  | _ -> Result.Error (Invalid_argument "Expr.time_of_value")
let int_list_of_value : value -> (int list) result = function
  | `List l -> Result.Ok (List.map (fun x -> match int_of_value x with Result.Ok i -> i | _ -> Printf.printf "Expr.int_list_of_value failure\n";assert false) l)
  | _ -> Result.Error (Invalid_argument "Expr.int_list_of_value")
let string_list_of_value : value -> (string list) result = function
  | `List l -> Result.Ok (List.map (fun x -> match string_of_value x with Result.Ok s -> s | _ -> Printf.printf "Expr.string_list_of_value failure\n"; assert false) l) 
  | _ -> Result.Error (Invalid_argument "Expr.string_list_of_value")
let fun_of_value : value -> (value -> value result) result = function
  | `Function f -> Result.Ok f
  | _ -> Result.Error (Invalid_argument "Expr.fun_of_value")
let list_of_value : value -> (value list) result = function
  | `List l -> Result.Ok (List.map (fun x -> x) l) 
  | _ -> Result.Error (Invalid_argument "Expr.string_list_of_value")

let regex_date = Str.regexp {|[0-3]?[0-9]\(/\|-\|\.\)\(\([0-1]?[0-9]\)\|\(jan\)\|\(fev\)\|\(mar\)\|\(avr\)\|\(jun\)\|\(jul\)\|\(aou\)\|\(oct\)\|\(nov\)\|\(dec\)\)\(/\|-\|\.\)-?[0-9]?[0-9]?[0-9]?[0-9]|}
let regex_time = Str.regexp {|\(\(\([0-1]?[0-9]\)\|\(2[0-3]\)\):[0-5]?[0-9]\(:[0-5]?[0-9]\)?\)\|\(\(\(0?[0-9]\)\|\(1[0-2]\)\):[0-5]?[0-9]\(:[0-5]?[0-9]\)?\(\(pm\)\|\(am\)\)\)|}

(* functions *)

module Funct =
  struct
    type t =
      [ `Uppercase 
      | `Lowercase 
      | `Initial 
      | `Length 
      | `Concat 
      | `Mirror  
      | `String_of_int 
      | `Date_of_string 
      | `Get_day 
      | `Get_month_in_letters 
      | `Get_month_in_numbers 
      | `Get_year 
      | `Time_of_string 
      | `Get_hours 
      | `Get_minutes 
      | `Get_seconds 
      | `Concat_list
      | `String_list_map
      | `String_of_list
      ]

    let uppercase (s : string) : string = String.uppercase_ascii s
    let lowercase (s : string) : string = String.lowercase_ascii s
    let initial (s : string) : string =
      if s = ""
      then ""
      else String.sub s 0 1
    let length (s : string) : int = String.length s
    let concat (s1 : string) (s2 : string) : string = s1 ^ s2
    let mirror (s : string) : string =
      let i = ref 0 in
      let l = ref [] in
      while !i < (String.length s) do
        l := (String.make 1 s.[!i]) :: !l;
        i := !i + 1;
      done;
      String.concat "" !l 
    let string_of_int (i : int) : string =
      string_of_int i
    let date_of_string (s : string) : int*int*int = 
      assert ((Str.string_match regex_date s 0) && (Str.match_end () = String.length s));
      let d, next_char = match int_of_string_opt (String.sub s 0 2) with
      | None -> int_of_string (String.sub s 0 1), 2
      | Some i -> i, 3 in
      Printf.printf "d:%d, nxt_chr:%d\n" d next_char;
      let m, next_char = match int_of_string_opt (String.sub s next_char 2) with
      | None -> (match int_of_string_opt (String.sub s next_char 1) with
                 | None -> (match (String.sub s next_char 3) with
                            | "jan" -> 1, next_char + 4
                            | "fev" -> 2, next_char + 4
                            | "mar" -> 3, next_char + 4
                            | "avr" -> 4, next_char + 4
                            | "mai" -> 5, next_char + 4
                            | "jun" -> 6, next_char + 4
                            | "jul" -> 7, next_char + 4
                            | "aou" -> 8, next_char + 4
                            | "sep" -> 9, next_char + 4
                            | "oct" -> 10, next_char + 4
                            | "nov" -> 11, next_char + 4
                            | "dec" -> 12, next_char + 4
                            | _ -> failwith "invalid month in date")
                 | Some i -> i, next_char+2)
      | Some i -> i, next_char+3 in
      Printf.printf "m:%d, nxt_chr:%d\n" m next_char;
      let y = int_of_string (String.sub s next_char (String.length s - next_char)) in
      Printf.printf "y:%d\n" y;
      (d, m, y)
    let get_day (date : int*int*int) : int =
      let d,_,_ = date in d
    let get_month_in_numbers (date : int*int*int) : int =
        let _,m,_ = date in m
    let get_month_in_letters (date : int*int*int) : string =
      let _,m,_ = date in
      match m with
      | 1 -> "jan"
      | 2 -> "fev"
      | 3 -> "mar"
      | 4 -> "avr"
      | 5 -> "mai"
      | 6 -> "jun"
      | 7 -> "jul"
      | 8 -> "aou"
      | 9 -> "sep"
      | 10 -> "oct"
      | 11 -> "nov"
      | 12 -> "dec"
      | _ -> failwith "invalid month in date"
    let get_year (date : int*int*int) : int =
      let _,_,y = date in y
    let time_of_string (s : string) : int*int*int =
      assert ((Str.string_match regex_time s 0) && (Str.match_end () = String.length s));
      let n = String.length s in
      let ampm =
        if s.[n-1] = 'm' then match s.[n-2] with 
                              | 'p' -> 12 
                              | 'a' -> 0
                              | _ -> assert false
        else 0 in
      let h, next_char = match int_of_string_opt (String.sub s 0 2) with
      | None -> int_of_string (String.sub s 0 1), 2
      | Some i -> i, 3 in
      let m, next_char = match int_of_string_opt (String.sub s next_char 2) with
      | None -> int_of_string (String.sub s next_char 1), next_char + 2
      | Some i -> i, next_char + 3 in
      let sec = match int_of_string_opt (String.sub s next_char 2) with
      | None -> (match int_of_string_opt (String.sub s next_char 1) with
                | None -> 0
                | Some i -> i)
      | Some i -> i in
    (h+ampm, m, sec)
    let get_hours (time : int*int*int) : int =
      let h,_,_ = time in h
    let get_minutes (time : int*int*int) : int =
        let _,m,_ = time in m
    let get_seconds (time : int*int*int) : int =
      let _,_,s = time in s
    let concat_list (l : string list) : string =
      String.concat "" l
    let string_of_list (l : string list) : string =
      String.concat " " l

    let rec apply_unary_op (op : t) (v : value) : value result =
      print_string "Expr.apply_unary_op\n";
      match fun_of_value v with
      | Result.Ok _ -> Result. Ok (`Function (fun x -> apply_unary_op op x))
      | Result.Error _ -> (
      match op with
        | `Uppercase ->
            let| s = string_of_value v in
            let res = uppercase s in
            Result.Ok (`String res)
        | `Lowercase ->
             let| s = string_of_value v in
             let res = lowercase s in
             Result.Ok (`String res)
        | `Initial ->
             let| s = string_of_value v in
             let res = initial s in
             Result.Ok (`String res)
        | `Length  ->
             let| s = string_of_value v in
             let res = length s in
             Result.Ok (`Int res)
        | `Mirror ->
             let| s = string_of_value v in
             let res = mirror s in
             Result.Ok (`String res)
        | `String_of_int ->
             let| i = int_of_value v in
             let res = string_of_int i in
             Result.Ok (`String res)
        | `Date_of_string ->
             let| s = string_of_value v in
             let res = date_of_string s in
             Result.Ok (`Date res)
        | `Get_day ->
             let| d = date_of_value v in
             let res = get_day d in
             Result.Ok (`Int res)
        | `Get_month_in_letters ->
             let| d = date_of_value v in
             let res = get_month_in_letters d in
             Result.Ok (`String res)
        | `Get_month_in_numbers ->
             let| d = date_of_value v in
             let res = get_month_in_numbers d in
             Result.Ok (`Int res)
        | `Get_year ->
             let| d = date_of_value v in
             let res = get_year d in
             Result.Ok (`Int res)
        | `Time_of_string ->
             let| s = string_of_value v in
             let res = time_of_string s in
             Result.Ok (`Time res)
        | `Get_hours ->
             let| t = time_of_value v in
             let res = get_hours t in
             Result.Ok (`Int res)
        | `Get_minutes ->
             let| t = time_of_value v in
             let res = get_minutes t in
             Result.Ok (`Int res)
        | `Get_seconds ->
             let| t = time_of_value v in
             let res = get_seconds t in
             Result.Ok (`Int res)
        | `Concat_list ->
            let| l = string_list_of_value v in
            let res = concat_list l in
            Result.Ok (`String res)
        | `String_of_list ->
            let| l = string_list_of_value v in
            let res = string_of_list l in
            Result.Ok (`String res)
        | _ -> Printf.printf "This is not a unary operator"; assert false)

    let apply_binary_op (op : t) (v1 : value) (v2 : value) : value result =
      match op with
      | `Concat ->
          let| s1 = string_of_value v1 in
          let| s2 = string_of_value v2 in
          let res = concat s1 s2 in
          Result.Ok (`String res)
      | `String_list_map ->
          let| f = fun_of_value v1 in
          let| l = list_of_value v2 in
          let res = List.map (fun x -> let Result.Ok (`String s) = f x in `String s) l in
          Result.Ok (`List res )    
      | _ -> Printf.printf "This is not a binary operator"; assert false
  end
                                                
(* expressions *)
     
type 'var expr =
  [ `Ref of 'var
  | `Lambda
  | `Unary of Funct.t * 'var expr
  | `Binary of Funct.t * 'var expr * 'var expr
  ]
let rec xp_expr (xp_var : 'var Xprint.xp) (print : Xprint.t) : 'var expr -> unit = function
  | `Ref p -> print#string "!"; xp_var print p
  | `Lambda -> print#string "x -> x"
  | `Unary(op, e) -> xp_funct print op; print#string "("; xp_expr xp_var print e; print#string ")"
  | `Binary(op, e1, e2) -> xp_funct print op; print#string "("; xp_expr xp_var print e1; xp_expr xp_var print e2; print#string ")"
and xp_funct (print : Xprint.t) : Funct.t -> unit = function
  | `Uppercase -> print#string "uppercase"
  | `Lowercase -> print#string "lowercase"
  | `Initial -> print#string "initial"
  | `Length -> print#string "length"
  | `Concat ->  print#string "concat"; 
  | `Mirror -> print#string "mirror"
  | `String_of_int -> print#string "string_of_int"
  | `Date_of_string -> print#string "date_of_string"
  | `Get_day -> print#string "get_day"
  | `Get_month_in_letters -> print#string "get_month_in_let"
  | `Get_month_in_numbers -> print#string "get_month_in_num"
  | `Get_year -> print#string "get_year"
  | `Time_of_string -> print#string "time_of_string"
  | `Get_hours -> print#string "get_hours"
  | `Get_minutes -> print#string "get_minutes"
  | `Get_seconds -> print#string "get_seconds"
  | `Concat_list -> print#string "concat_list"
  | `String_list_map -> print#string "string_list_map"
  | `String_of_list -> print#string "string_of_list"

  
let rec eval (lookup : 'var -> value result) : 'var expr -> value result = function
  | `Ref x -> lookup x
  | `Lambda -> Result.Ok (`Function (fun x -> Result.Ok x))
  | `Unary(op, e) ->
      let| v = eval lookup e in
      Funct.apply_unary_op op v
  | `Binary(op, e1, e2) ->
      let| v1 = eval lookup e1 in
      let| v2 = eval lookup e2 in
      Funct.apply_binary_op op v1 v2
  

let dl_funct = Mdl.Code.uniform 19 (* 19 functions *)
     
let rec dl_expr (dl_var : 'var -> dl) (e : 'var expr) : dl =
  let nb_funct = dl_expr_stats e in
  Mdl.Code.universal_int_star nb_funct
  +. dl_expr_aux dl_var nb_funct e
and dl_expr_aux dl_var nb_funct = function
  | `Lambda ->
       0.
  | `Ref p ->
       assert (nb_funct = 0); (* must be a ref *)
       dl_var p
  | `Unary(op, e1) ->
       dl_funct
       +. dl_expr_aux dl_var (nb_funct - 1) e1
  | `Binary(op, e1,e2) ->
       let nb1 = dl_expr_stats e1 in
       let nb2 = dl_expr_stats e2 in
       assert (nb1 + nb2 + 1 = nb_funct);
       dl_funct
       +. Mdl.Code.uniform (nb1 + 1) (* choosing split of functions between e1 and e2 *)
       +. dl_expr_aux dl_var nb1 e1
       +. dl_expr_aux dl_var nb2 e2
and dl_expr_stats : 'var expr -> int = function (* counting function applications *)
  | `Ref _ -> 0
  | `Lambda -> 0
  | `Unary(op, e1) -> 1 + dl_expr_stats e1
  | `Binary(op, e1, e2) -> 1 + dl_expr_stats e1 + dl_expr_stats e2
                     
(* expression sets : idea taken from FlashMeta *)
    
type 'var exprset = 'var expritem list
and 'var expritem =
  [ `Lambda
  | `Ref of 'var
  | `Unary of Funct.t * 'var exprset 
  | `Binary of Funct.t * 'var exprset * 'var exprset ]

let rec exprset_to_seq (es : 'var exprset) : 'var expr Myseq.t =
  let* item = Myseq.from_list es in
  expritem_to_seq item
and expritem_to_seq : 'var expritem -> 'var expr Myseq.t = function
  | `Lambda -> Myseq.return (`Lambda)
  | `Ref x -> Myseq.return (`Ref x)
  | `Unary (op, es1) ->
      let* e1 = exprset_to_seq es1 in
      Myseq.return (`Unary(op, e1))
  | `Binary (op,es1,es2) ->
      let* e1 = exprset_to_seq es1 in
      let* e2 = exprset_to_seq es2 in
      Myseq.return (`Binary (op,e1,e2))
  
let rec exprset_inter (es1 : 'var exprset) (es2 : 'var exprset) : 'var exprset =
  List.fold_left
    (fun res item1 ->
      List.fold_left
        (fun res item2 ->
          match expritem_inter item1 item2 with
          | None -> res
          | Some item -> item::res)
        res es2)
    [] es1
and expritem_inter (item1 : 'var expritem) (item2 : 'var expritem) : 'var expritem option =
  match item1, item2 with
  | `Lambda, `Lambda -> Some `Lambda
  | `Ref x1, `Ref x2 when x1 = x2 -> Some (`Ref x1)
  | `Unary (op1, e1), `Unary (op2, e2) when op1 = op2->
     (match exprset_inter e1 e2 with
      | [] -> None
      | e -> Some (`Unary (op1, e)))
  | `Binary (op1,e1,f1), `Binary (op2,e2,f2) when op1 = op2 ->
     (match exprset_inter e1 e2, exprset_inter f1 f2 with
      | [], _ | _, [] -> None
      | e, f -> Some (`Binary (op1,e,f)))
  | _ -> None

let rec exprset_inter_list (esl1 : 'var exprset list) (esl2 : 'var exprset list) : 'var exprset list =
  List.fold_left
    (fun res es1 ->
      List.fold_left
        (fun res es2 ->
          match exprset_inter es1 es2 with
          | [] -> res
          | es -> es::res)
        res esl2)
    [] esl1


(* indexes : idea inspired from FlashMeta *)

module MakeIndex (V : Map.OrderedType) =
  struct
    module M = Map.Make(V)
                       
    type 'var t = 'var exprset M.t

    let empty = M.empty
                
    let bind (v : V.t) (item : 'var expritem) (index : 'var t) : 'var t =
      M.update v
        (function
         | None -> Some [item]
         | Some exprs -> Some (item :: exprs))
        index

    let find_opt = M.find_opt

    let fold = M.fold

    let iter = M.iter
  end

  
module StringIndex = MakeIndex(struct type t = string let compare = Stdlib.compare end)
(*type 'var string_index = 'var exprset StringIndex.t
let bind_string (v : string) (item : 'var expritem) (index : 'var string_index) : 'var string_index =
  StringIndex.update v
    (function
     | None -> Some [item]
     | Some exprs -> Some (item :: exprs))
    index*)

module IntIndex = MakeIndex(struct type t = int let compare = Stdlib.compare end)
(*type 'var int_index = 'var exprset IntIndex.t                
let bind_int (v : int) (item : 'var expritem) (index : 'var int_index) : 'var int_index =
  IntIndex.update v
    (function
     | None -> Some [item]
     | Some exprs -> Some (item :: exprs))
    index*)

module DateIndex = MakeIndex(struct type t = int*int*int let compare = Stdlib.compare end)

module TimeIndex = MakeIndex(struct type t = int*int*int let compare = Stdlib.compare end)

module StringListIndex = MakeIndex(struct type t = string list let compare = Stdlib.compare end)
           
type 'var index =
  { by_string : 'var StringIndex.t;
    by_int : 'var IntIndex.t;
    by_date : 'var DateIndex.t;
    by_time : 'var TimeIndex.t;
    by_string_list : 'var StringListIndex.t 
  }

let index_lookup (v : value) (index : 'var index) : 'var exprset =
  match v with
  | `String s ->
     (match StringIndex.find_opt s index.by_string with
      | None -> []
      | Some exprs -> exprs)
  | `Int i ->
     (match IntIndex.find_opt i index.by_int with
      | None -> []
      | Some exprs -> exprs) 
  | `List l -> 
      (let res = (match string_list_of_value (`List l) with Result.Ok l -> l | Result.Error _ -> Printf.printf "Expr.index_lookup failure (1)\n"; assert false) in
      match StringListIndex.find_opt res index.by_string_list with
      | None -> []
      | Some exprs -> exprs)
  | _ -> Printf.printf "Expr.index_lookup failure (2)\n"; assert false  
  
let make_index (bindings : ('var * value) list) : 'var index =
  Printf.printf "Expr.make_index\n";
  let index_string, index_int, index_date, index_time, index_string_list =
    List.fold_left
      (fun (idx_s,idx_i,idx_d,idx_t,idx_sl) (x,v) ->
        match v with
        | `String s -> StringIndex.bind s (`Ref x) idx_s, idx_i, idx_d, idx_t, idx_sl
        | `Int i -> idx_s, IntIndex.bind i (`Ref x) idx_i, idx_d, idx_t, idx_sl
        | `Date d -> idx_s, idx_i, DateIndex.bind d (`Ref x) idx_d, idx_t, idx_sl
        | `Time t -> idx_s, idx_i, idx_d, TimeIndex.bind t (`Ref x) idx_t, idx_sl
        | `List l -> idx_s, idx_i, idx_d, idx_t, StringListIndex.bind (let Result.Ok l = string_list_of_value (`List l) in l) (`Ref x) idx_sl
        | `Function f -> idx_s, idx_i, idx_d, idx_t, idx_sl)
      (StringIndex.empty, IntIndex.empty, DateIndex.empty, TimeIndex.empty, StringListIndex.empty) bindings in
  let index_int =
    Printf.printf "Expr.make_index.index_int (from lengths of index_string)\n";
    StringIndex.fold
      (fun s exprs res ->
        IntIndex.bind (String.length s) (`Unary(`Length, exprs)) res)
      index_string index_int in
  let index_date =
    Printf.printf "Expr.make_index.index_date (from index_string)\n";
    StringIndex.fold
      (fun s exprs res ->
        if (Str.string_match regex_date s 0) && (Str.match_end () = String.length s) then
          (Printf.printf "wtf are you doing?? %s\n" s;
          DateIndex.bind (Funct.date_of_string s) (`Unary(`Date_of_string, exprs)) res)
        else
          res) index_string index_date in
  let index_int = 
    Printf.printf "Expr.make_index.index_int (from index_date)\n";
    DateIndex.fold
      (fun d exprs res ->
        let res = IntIndex.bind (Funct.get_day d) (`Unary(`Get_day, exprs)) res in
        let res = IntIndex.bind (Funct.get_month_in_numbers d) (`Unary(`Get_month_in_numbers, exprs)) res in
        let res = IntIndex.bind (Funct.get_year d) (`Unary(`Get_year, exprs)) res in
        res)
      index_date index_int in
  let index_time =
    Printf.printf "Expr.make_index.index_time (from index_string)\n";
    StringIndex.fold
      (fun s exprs res ->
        Printf.printf "s:%s\n" s;
        if (Str.string_match regex_time s 0) && (Str.match_end () = String.length s)
          then TimeIndex.bind (Funct.time_of_string s) (`Unary(`Time_of_string, exprs)) res
      else
        res) index_string index_time in
  let index_int = 
    Printf.printf "Expr.make_index.index_int (from index_time)\n";
    TimeIndex.fold
      (fun t exprs res ->
        let res = IntIndex.bind (Funct.get_hours t) (`Unary(`Get_hours, exprs)) res in
        let res = IntIndex.bind (Funct.get_minutes t) (`Unary(`Get_minutes, exprs)) res in
        let res = IntIndex.bind (Funct.get_seconds t) (`Unary(`Get_seconds, exprs)) res in
        res)
      index_time index_int in
  let index_string_list =
    Printf.printf "Expr.make_index.index_string_list (initiales)\n";
    StringListIndex.fold
      (fun l exprs res ->
           let lambda = (`Lambda :> 'var expritem) in
           let initiale_fun = ([`Unary(`Initial, [lambda])] :> 'var exprset) in
           let uppercase_fun = ([`Unary(`Uppercase, [lambda])] :> 'var exprset) in
           let lowercase_fun = ([`Unary(`Lowercase, [lambda])] :> 'var exprset) in
           let mirror_fun = ([`Unary(`Mirror, [lambda])] :> 'var exprset) in
           let res = StringListIndex.bind (List.map Funct.initial l) (`Binary(`String_list_map, initiale_fun, exprs)) res in
           let res = StringListIndex.bind (List.map Funct.uppercase l) (`Binary(`String_list_map, uppercase_fun, exprs)) res in
           let res = StringListIndex.bind (List.map Funct.lowercase l) (`Binary(`String_list_map, lowercase_fun, exprs)) res in
           let res = StringListIndex.bind (List.map Funct.mirror l) (`Binary(`String_list_map, mirror_fun, exprs)) res in
           res)
      index_string_list index_string_list in
  let index_string =
    Printf.printf "Expr.make_index.index_string (from index_date)\n";
    DateIndex.fold
      (fun d exprs res ->
          let s = Funct.get_month_in_letters d in
          StringIndex.bind s (`Unary(`Get_month_in_letters, exprs)) res)
      index_date index_string in
  let index_string =
    Printf.printf "Expr.make_index.index_string (from index_int)\n";
    IntIndex.fold
      (fun i exprs res ->
          let s = Funct.string_of_int i in
          StringIndex.bind s (`Unary(`String_of_int, exprs)) res)
      index_int index_string in
  let index_string =
    Printf.printf "Expr.make_index.index_string (from index_string: initiales, uppercases, lowercases and mirrors)\n";
    StringIndex.fold
      (fun s exprs res ->
        let res = StringIndex.bind (Funct.initial s) (`Unary(`Initial, exprs)) res in
        let res = StringIndex.bind (Funct.uppercase s) (`Unary(`Uppercase, exprs)) res in
        let res = StringIndex.bind (Funct.lowercase s) (`Unary(`Lowercase, exprs)) res in
        let res = StringIndex.bind (Funct.mirror s) (`Unary(`Mirror, exprs)) res in
        res)
      index_string index_string in
  let index_string = 
    Printf.printf "Expr.make_index.index_string (concat string list)\n";
    StringListIndex.fold
      (fun l exprs res -> 
         let res = StringIndex.bind (Funct.concat_list l) (`Unary(`Concat_list, exprs)) res in
         let res = StringIndex.bind (Funct.string_of_list l) (`Unary(`String_of_list, exprs)) res in
         res)
      index_string_list index_string in
  let index_string =
    StringIndex.fold
      (fun s1 exprs1 res ->
        StringIndex.fold
          (fun s2 exprs2 res ->
            res |> StringIndex.bind (s1 ^ s2) (`Binary(`Concat, exprs1, exprs2)))
          index_string res)
      index_string index_string in
  { by_string = index_string;
    by_int = index_int;
    by_date = index_date;
    by_time = index_time;
    by_string_list = index_string_list;}
