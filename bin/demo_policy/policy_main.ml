open Policy_def
module DemoCombination = Policy.Combination.StatesCombination(Policy_def.Demo);;  
module TypedTools = Policy.Tools.Make(Policy_def.Demo)
module IntMap = Map.Make(Int)
open Policy_matrix_def
let construct_trans_matrix simplified_trans_file num_of_states = 
  let to_map_update = 
    fun new_elem curr_loi -> match curr_loi with 
    | None -> Some [new_elem]
    | Some loi -> Some (new_elem :: loi) in
  let from_map_update = 
    fun (to_id,new_elem) curr_map_of_loi -> 
      match curr_map_of_loi with 
      | None -> Some (IntMap.singleton to_id [new_elem])
      | Some m -> Some (IntMap.update to_id (to_map_update new_elem) m) in
  let loaded_trans = Csv.load simplified_trans_file in
  let result_as_map,_ = List.fold_left
    (
    fun (res_m,tran_fun_id) los -> 
      match List.length los with
      | 2 -> 
        let from_id = List.nth los 0 |> int_of_string
        and to_id = List.nth los 1 |> int_of_string in
          IntMap.update from_id ( from_map_update (to_id, (IntMap.find tran_fun_id mof) )) res_m , tran_fun_id+1
      | _ -> raise (Invalid_argument "Incorrect csv file provided")
    )
    (IntMap.empty,0)
    loaded_trans in
  let result = Array.mapi 
    (
      fun from_id combi_array -> 
        Array.mapi 
        (
          fun to_id _ -> 
            let row_opt = IntMap.find_opt from_id result_as_map in
            match row_opt with 
            | None -> []
            | Some row -> 
              let elem_opt = IntMap.find_opt to_id row in
              match elem_opt with
              | None -> []
              | Some elem -> elem
        ) 
        combi_array 
    )
    (Array.make_matrix num_of_states num_of_states []) in
    result


let ns = 11
let init_agent_state = (1,0)
let init_set_of_actions = SetOfActions.empty
let init_state =  (Demo.Reachable (init_agent_state,init_set_of_actions)) 
let init_elem = TypedTools.make_init_state_matrix_singleton ~num_of_states:ns init_state
let mf_m = construct_trans_matrix "simplified_trans.csv" 11
(*
let mf_m =
  [|
    [|[f_null];[f0];[f1];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null]|];
    [|[f3];[f_null];[f_null];[f2];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null]|];
    [|[f5];[f_null];[f_null];[f_null];[f4];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f6];[f_null];[f_null];[f_null];[f_null];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f8];[f_null];[f7];[f_null];[f_null];[f_null];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f10];[f_null];[f9];[f_null];[f_null];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f11];[f12];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f14];[f_null];[f_null];[f13]|];
    [|[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f_null];[f15];[f_null]|]
  |]
*)
let mf = {DemoCombination.matrix = mf_m; num_of_states=ns}

let res,is_reached = TypedTools.multiply_until_state_is_reached 
  ~filter_fun:(fun x -> if x <> Unreachable then true else false) 
  ~limit:7
  ~desired_state_id:8
  init_elem
  mf;;
if is_reached then (
  print_endline "first reach of 8th state";
  DemoCombination.print_km res.matrix
  )
else
  print_endline "could not find a walk to 8th state";;
