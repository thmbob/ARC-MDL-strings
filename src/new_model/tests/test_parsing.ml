open Types_utilities
open Parsing
open Printing

module Myseq = struct
  include Myseq
  let uncons seq = 
    match seq () with
    | Nil -> None
    | Cons(h,t) -> Some (h, t)
end

let path m s = 
  let parse_seq = (row_parse m s) in
  Printf.printf "\nParsing result :\n";
  Myseq.iter print_row_data parse_seq
  
let () =
  let sl = ["Karim 25   278 45    12/12/678";"ehfhiae ezgz"] in
  let m = [[AnyToken;RegexToken(Letters);ConstToken(" ");ListToken(RegexToken(Nums));RegexToken(Blank);RegexToken(Date)];[AnyToken;RegexToken(Letters)]] in
  path m sl
  