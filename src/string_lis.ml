
open Js_of_ocaml

let _ = Common.prof_on := false (* required because primitive unix_times not supported by js_of_ocaml *)

exception TODO

let ( let| ) res f = Result.bind res f [@@inline]

(* binding operator to force x's effects to take place before running f *)
(* typical case: x changes the DOM, f () is a long computation *)
let ( let> ) x f =
  ignore x;
  ignore
    (Dom_html.window##setTimeout
       (Js.wrap_callback f)
       0.)
  
(* ---- LIS -------- *)
        
type 'a triple = 'a Model.triple
        
type task_input = (string * Task.task) Focus.input (* name, data *)
                
type arc_state =
  { name : string; (* task name *)
    task : Task.task; (* task *)
    norm_dl_model_data : Model.pairs_reads -> Model.dl triple triple;
    refinement : Model.refinement; (* previous refinement *)
    model : Model.model; (* current model *)
    prs : Model.pairs_reads; (* pair reads *)
    dsri : Model.rows_reads; (* input reads *)
    dsro : Model.rows_reads; (* output reads *)
    dls : Mdl.bits Model.triple Model.triple; (* DL components *)
    norm_dls : Mdl.bits Model.triple Model.triple; (* normalized DL components *)
    norm_dl : Mdl.bits; (* global normalized DL *)
    mutable suggestions : arc_suggestion list;
  }
and arc_suggestion =
  | InputTask of task_input
  | ResetTask
  | RefinedState of arc_state * bool (* compressive *)

type arc_focus = arc_state
               
type arc_extent = arc_state

let rec state_of_model (name : string) (task : Task.task) norm_dl_model_data (refinement : Model.refinement) (model : Model.model) : (arc_state, exn) Result.t =
  let| prs = Model.read_pairs model task.Task.train in
  let dsri, dsro = Model.split_pairs_read prs in
  let dls = Model.dl_model_data prs in
  let (_, _, (_,_,norm_dl) as norm_dls) = norm_dl_model_data prs in
  Result.Ok
    { name; task;
      norm_dl_model_data;
      refinement;
      model;
      prs; dsri; dsro;
      dls;
      norm_dls;
      norm_dl;
      suggestions = [] }  
               
let name0 = "default: name + birthdate"
let task0 =
  let open Task in
  { train = [ {input = ["marie Dupont - 19/4/2000"]; output = ["M. Dupont, 2000"]};
              {input = ["Jean Martin - 12/8/1999"]; output = ["J. Martin, 1999"]} ];
    test = [ {input = ["Marc Bonpain - 13/12/2002"]; output = ["M. Bonpain, 2002"]} ] }

let initial_focus (name : string) (task : Task.task) : arc_focus =
  let norm_dl_model_data = Model.make_norm_dl_model_data () in
  match state_of_model name task norm_dl_model_data Model.RInit (Model.init_model task) with
  | Result.Ok s -> s
  | Result.Error exn -> raise exn

        
class arc_place (lis : arc_lis) (focus : arc_focus) =
object
  inherit [arc_lis,arc_focus,arc_extent,arc_suggestion] Lis.place lis focus

  method eval k_extent k_suggestions =
    k_extent focus;
    if focus.suggestions = [] then (
      Jsutils.firebug "Computing suggestions...";
      let _, suggestions = (* selecting up to [refine_degree] compressive refinements, keeping other for information *)
        Model.model_refinements focus.refinement focus.model focus.prs focus.dsri focus.dsro
        |> Myseq.fold_left
             (fun (quota_compressive,suggestions as res) (r,m) ->
               if quota_compressive <= 0
               then res (* TODO: stop generating sequence *)
               else
                 match state_of_model focus.name focus.task focus.norm_dl_model_data r m with
                 | Result.Ok state ->
                    if state.norm_dl < focus.norm_dl
                    then (quota_compressive - 1, state::suggestions)
                    else (quota_compressive, state::suggestions)
                 | Result.Error _ -> res)
             (!Model.max_refinements, []) in
      let suggestions = (* sorting in increasing DL *)
        suggestions
        |> List.rev (* to preserve ordering from sequence *) 
        |> List.sort
             (fun s1 s2 -> Stdlib.compare s1.norm_dl s2.norm_dl) in
      let suggestions =
        InputTask (new Focus.input (name0,task0))
        :: ResetTask
        :: List.map (fun s ->
               let compressive = s.norm_dl < focus.norm_dl in
               RefinedState ((s :> arc_state), compressive))
             suggestions in
      Jsutils.firebug "Suggestions computed";
      focus.suggestions <- suggestions
    );
    let suggestions = (* suggestion list in Fablis format *)
      focus.suggestions
      |> List.map (fun sugg -> `Sugg sugg) in
    k_suggestions [suggestions]

  method activate = function
    | InputTask i ->
       let name, task = i#get in
       let state = initial_focus name task in
       Some (new arc_place lis state)
    | ResetTask ->
       Some (new arc_place lis (initial_focus focus.name focus.task))
    | RefinedState (s,_) ->
       Some (new arc_place lis s)

  method abort = ()

  method json = raise TODO

  method results = raise TODO
end

and arc_lis =
object (self)
  inherit [arc_place] Lis.lis

  method initial_place =
    let state0 = initial_focus name0 task0 in
    new arc_place (self :> arc_lis) state0

  method place_of_json json = raise TODO
end

let make_lis (args : (string * string) list) =
  new arc_lis

(* ------- WEBAPP --------- *)
  
type arc_word

type arc_input = [`Task of task_input]
   
let html_dl dl =
  Printf.sprintf "%.3f" dl
   
let xml_of_focus focus =
  let (mi,mo,m), (di,do_,d), (mdi,mdo,md) = focus.dls in
  [Syntax.Block
     [[Syntax.Kwd (Printf.sprintf "Task %s" focus.name)];
      [Syntax.Kwd (Printf.sprintf "DL = %f" focus.norm_dl)];
      [Syntax.Kwd (Printf.sprintf "DL = %.3f = %.3fm + %.3fd = (%.3fmi + %.3fmo) + (%.3fdi + %.3fdo) = %.3fi + %.3fo" md m d mi mo di do_ mdi mdo)];
      [Syntax.Kwd (Model.string_of_row_model focus.model.input_model)];
      [Syntax.Kwd " ➜ "];
      [Syntax.Kwd (Model.string_of_row_model focus.model.output_model)]]]
  
let html_of_word (w : arc_word) : Html.t = assert false

let html_info_of_input (input : arc_input) : Html.input_info =
  match input with
  | `Task input ->
     Html.fileElt_info
       (Some ".json")
       (fun (fname,contents) k ->
         let task_name = Filename.chop_extension (Filename.basename fname) in
         let json = Yojson.Safe.from_string contents in
         let task = Task.task_of_json json in
         input#set (task_name, task);
         k ())
                                         
let html_of_suggestion ~input_dico = function
  | InputTask i ->
     let info = html_info_of_input (`Task i) in
     let key = input_dico#add info in
     Html.html_of_input_info key info ^ " a task"
  | ResetTask ->
     "reset current task"
  | RefinedState (s,compressive) ->
     Html.span ~classe:(if compressive then "compressive" else "non-compressive")
       (Printf.sprintf "(%f" s.norm_dl
        ^ Model.string_of_refinement s.refinement)

let html_of_row_from_string_list (ls : string list) =
  String.concat "</br>"
    (List.map (Xprint.to_string Model.xp_string) ls)

let html_of_row_from_data data =
  Model.string_of_row_data data

let html_row_pair html_i html_o =
  html_i ^ "<br/> ➜ <br/>" ^ html_o
    
type col = ColExample | ColDescr | ColPred
type cell =
  | Example of string list * string list
  | Descr of Model.row_read * Model.row_read
  | Pred of string list * (Model.row_data * string list) list (* expected grid, all preds *)
  | Error of string
                                    
let html_of_cell : cell -> Html.t = function
  | Example (lsi,lso) ->
     html_row_pair
       (html_of_row_from_string_list lsi)
       (html_of_row_from_string_list lso)
  | Descr (ri,ro) ->
     let (_, d_i, dli : Model.row_read) = ri in
     let (_, d_o, dlo : Model.row_read) = ro in
     html_row_pair
       (html_of_row_from_data d_i)
       (html_of_row_from_data d_o)
     ^ Printf.sprintf "<br/>DL = %.3f = %.3fi + %.3fo" (dli +. dlo) dli dlo
  | Pred (expected_so, l_di_so) ->
     String.concat ""
       (List.map
          (fun (d_i,lso) ->
            html_row_pair
              (html_of_row_from_data d_i)
              (html_of_row_from_string_list lso))
          l_di_so)
  | Error msg -> Jsutils.escapeHTML msg
        
let w_focus : (arc_word, unit, arc_focus) Widget_focus.widget =
  new Widget_focus.widget
    ~id:"lis-focus"
    ~html_of_word

let w_suggestions : arc_suggestion Widget_suggestions.widget =
  new Widget_suggestions.widget
    ~id:"lis-suggestions"
    ~html_of_suggestion

let cols = [ColExample, (); ColDescr, (); ColPred, ()]
let w_results : (col, unit, cell) Widget_table.widget =
  new Widget_table.widget
    ~id:"lis-results"
    ~html_of_column:(fun (col,()) ->
      let html =
        match col with
        | ColExample -> "Example"
        | ColDescr -> "Description"
        | ColPred -> "Prediction" in
      None, None, None, html)
    ~html_of_cell


let render_place place k =
  let get_pred ~test m lsi lso =
    match Model.apply_model m lsi with
    | Result.Ok [] -> Error "No valid prediction"
    | Result.Ok l_di_sopred -> Pred (lso, if test then l_di_sopred else [List.hd l_di_sopred])
    | Result.Error exn -> Error (Printexc.to_string exn)
  in
 Jsutils.jquery "#lis-suggestions" (fun elt_lis ->
  let> _ = Jsutils.toggle_class elt_lis "computing" in (* turn on *)
  let xml = xml_of_focus place#focus in
  w_focus#set_syntax xml;
  place#eval
    (fun ext ->
      let l_bindings =
        List.map
          (fun pair ->
            let ({input=lsi; output=lso} : Task.pair) = pair in
            let pred = get_pred ~test:true ext.model lsi lso in
            [ ColExample, Example (lsi,lso);
              ColPred, pred ])
          ext.task.test in
      let l_bindings =
        List.fold_right2
          (fun pair reads l_bindings ->
            let ({input=lsi; output=lso} : Task.pair) = pair in
            let descr =
              match reads with
              | (in_r,out_r,dl)::_ -> Descr (in_r,out_r) (* using only first read *)
              | [] -> Error "No valid description" in
            let pred = get_pred ~test:false ext.model lsi lso in
            let row =
              [ ColExample, Example (lsi,lso);
                ColDescr, descr;
                ColPred, pred ] in
            row::l_bindings)
          ext.task.train ext.prs.Model.reads l_bindings in
      w_results#set_contents cols l_bindings)
    (fun suggestions ->
      w_suggestions#set_suggestions ["col-md-12 col-xs-12"] suggestions;
      let suggestion_handler =
        (fun sugg ->
          match place#activate sugg with
          | Some p -> k ~push_in_history:true p
          | None -> assert false) in
      w_suggestions#on_suggestion_selection suggestion_handler;
      let _on = Jsutils.toggle_class elt_lis "computing" in (* turn off *)
      ()))


let handle_document_keydown ev place k =
  false

let error_message : exn -> string = function
  | exn -> "Unexpected error: " ^ Printexc.to_string exn

let _ =
  Jsutils.firebug "Start...";
  Webapp.start
    ~make_lis
    ~render_place
    ~handle_document_keydown
    ~error_message

