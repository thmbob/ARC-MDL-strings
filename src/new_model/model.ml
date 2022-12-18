open Types_utilities
open Parsing
open Dl_compute
open Environment
open Writing
open Reading
open Refinement
open Task

(* pairs/example *)

let row_model0 (row_size : int) : row_model = List.init row_size (fun _ -> [AnyToken])
let row_data0 (row_size : int) = List.init row_size (fun _ -> [])

let init_model (t : Task.task) =
  { input_model = row_model0 (Task.input_row_size t);
    output_model = row_model0 (Task.output_row_size t) }

let read_pairs ?(env = row_data0 0) (m : model) (pairs : Task.pair list) : pairs_reads result =
  Common.prof "Model.read_pairs" (fun () ->
  (* takes model, input env+docs, output docs *)
  let dl_mi = dl_row_model ~env:[] m.input_model in    
  let dl_mo = dl_row_model ~env:m.input_model m.output_model in
  let| inputs_reads_reads =
    pairs
    |> list_map_result
         (fun {input; output} ->
           let| input_reads =
             read_row ~env m.input_model input in (* no diff allowed during training *)
           let| pair_reads = 
             let+|+ (envi,ddi,dli as ri) = Result.Ok input_reads in      
             let+|+ (envo,ddo,dlo as ro) =
               read_row ~env:ddi m.output_model output in
               Printf.printf "just for debug... just checking...\n";
             let dl = dli +. dlo in
             Result.Ok [(ri,ro,dl)] in
           let pair_reads =
             pair_reads
             |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
           (* TODO |> limit_dl (fun (_,_,dl) -> dl) *) in (* bounding by dl_factor *) 
           Result.Ok (input_reads, pair_reads)) in
  let inputs_reads, reads = List.split inputs_reads_reads in
  Result.Ok {dl_mi; dl_mo; inputs_reads; reads})

let dl_model_data (psr : pairs_reads) : dl triple triple = (* QUICK *)
  let lmi = psr.dl_mi in
  let lmo = psr.dl_mo in
  let ldi, ldo =
    List.fold_left
      (fun (ldi,ldo) ->
        function
        | ((_,_,dli),(_,_,dlo),dl)::_ -> (ldi +. dli, ldo +. dlo)
        | _ -> assert false)
      (0.,0.) psr.reads in
  let ldi, ldo = !alpha *. ldi, !alpha *. ldo in
  let lmdi = lmi +. ldi in
  let lmdo = lmo +. ldo in
  (lmi, lmo, lmi +. lmo),
  (ldi, ldo, ldi +. ldo),
  (lmdi, lmdo, lmdi +. lmdo)

let make_norm_dl_model_data () : pairs_reads -> dl triple triple =
  let lmdi0 = ref (-1.) in
  let lmdo0 = ref (-1.) in
  fun psr ->
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi,lmdo,lmd) =
    dl_model_data psr in
  let () = (* setting initial DLs *)
    if !lmdi0 < 0.
    then ( lmdi0 := lmdi; lmdo0 := lmdo ) in
  let nlmi, nldi, nlmdi = lmi /. !lmdi0, ldi /. !lmdi0, lmdi /. !lmdi0 in
  let nlmo, nldo, nlmdo = lmo /. !lmdo0, ldo /. !lmdo0, lmdo /. !lmdo0 in
  (nlmi, nlmo, nlmi +. nlmo),
  (nldi, nldo, nldi +. nldo),
  (nlmdi, nlmdo, nlmdi +. nlmdo)

let split_pairs_read (prs : pairs_reads) : rows_reads * rows_reads =
  let project_reads proj =
    List.map
      (fun pair_reads ->
        pair_reads
        |> List.map proj)
      prs.reads in
  let inputs_reads = project_reads (fun (dri,_,_) -> dri) in
  let outputs_reads = project_reads (fun (_,dro,_) -> dro) in
  let dsri = { dl_m = prs.dl_mi; reads = inputs_reads } in
  let dsro = { dl_m = prs.dl_mo; reads = outputs_reads } in
  dsri, dsro


let apply_model ?(env = []) (m : model) (row_i : string list) : ((row_data * string list) list, exn) Result.t =
  Common.prof "Model.apply_model" (fun () ->
  let+|+ _, di, _ =
    read_row ~env m.input_model row_i in
  let| row_o =
    write_row ~env:di m.output_model in
  Result.Ok [(di, row_o)])

let model_refinements (last_r : refinement) (m : model) (prs : pairs_reads) (dsri : rows_reads) (dsro : rows_reads) : (refinement * model) Myseq.t =
  Myseq.concat (* TODO: rather order by estimated dl *)
    [ (let* v, shift, ri, dli', mi = row_refinements ~nb_env_paths:0 ~dl_M:prs.dl_mi m.input_model dsri in
       Myseq.return (Rinput (v,ri,dli'), {input_model = mi ; output_model = row_shift_ref m.output_model v shift}));
      (let* v, _, ro, dlo', mo = row_refinements ~nb_env_paths:(dl_row_model_env_stats m.input_model) ~dl_M:prs.dl_mo m.output_model dsro in
       Myseq.return (Routput (v,ro,dlo'), {m with output_model = mo})) ]