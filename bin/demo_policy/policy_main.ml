open Policy_def
module DemoCombination = Policy.Combination.StatesCombination(Policy_def.Demo);;  
module TypedTools = Policy.Tools.Make(Policy_def.Demo)

let ns = 11
let init_agent_state = (1,0)
let init_set_of_actions = SetOfActions.empty
let init_state =  (Demo.Reachable (init_agent_state,init_set_of_actions)) 
let init_elem = TypedTools.make_init_state_matrix_singleton ~num_of_states:ns init_state
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
let mf = {DemoCombination.matrix = mf_m; num_of_states=ns}

let res,is_reached = TypedTools.multiply_until_state_is_reached 
  ~filter_fun:(fun x -> if x <> Unreachable then true else false) 
  ~limit:777
  ~desired_state_id:8
  init_elem
  mf;;
if !is_reached then (
  print_endline "first reach of 8th state";
  DemoCombination.print_km res.matrix
  )
else
  print_endline "could not find a walk to 8th state";;
