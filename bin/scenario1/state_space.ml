open Bigraph
open Tracking_bigraph
open State_space_def

let deact_uav_f_rnm = Fun.empty |> Fun.add 0 0 |> Fun.add 1 1
let deact_uav_react = TBrs.parse_react "deact_uav" ~lhs:deact_uav_lhs ~rhs:deact_uav_rhs ~f_sm:None ~f_rnm:deact_uav_f_rnm
let deploy_data_f_rnm = Fun.empty |> Fun.add 0 0 |> Fun.add 1 1 |> Fun.add 2 2
let deploy_data_react = TBrs.parse_react "deploy_data" ~lhs:deploy_data_lhs ~rhs:deploy_data_rhs ~f_sm:None ~f_rnm:deploy_data_f_rnm
let downloadData_f_rnm = Fun.empty |> Fun.add 0 0 |> Fun.add 1 1 |> Fun.add 2 3 |> Fun.add 3 2
let downloadData_react = TBrs.parse_react "downloadData" ~lhs:downloadData_lhs ~rhs:downloadData_rhs ~f_sm:None ~f_rnm:downloadData_f_rnm
let move_into_base_f_rnm = Fun.empty |> Fun.add 0 0 |> Fun.add 1 2 |> Fun.add 2 1
let move_into_base_f_sm = Fun.empty |> Fun.add 0 1 |> Fun.add 1 0 |> Fun.add  2 2 
let move_into_base_react = TBrs.parse_react "move_into_base" ~lhs:move_into_base_lhs ~rhs:move_into_base_rhs ~f_sm:(Some move_into_base_f_sm) ~f_rnm:move_into_base_f_rnm
let move_out_of_base_f_rnm = Fun.empty |> Fun.add 0 0 |> Fun.add 1 2 |> Fun.add 2 1
let move_out_of_base_f_sm = Fun.empty |> Fun.add 0 1 |> Fun.add 1 0 |> Fun.add  2 2 
let move_out_of_base_react = TBrs.parse_react "move_out_of_base" ~lhs:move_out_of_base_lhs ~rhs:move_out_of_base_rhs ~f_sm:(Some move_out_of_base_f_sm) ~f_rnm:move_out_of_base_f_rnm
let move_f_rnm = Fun.empty |> Fun.add 0 0 |> Fun.add 1 2 |> Fun.add 2 1
let move_f_sm = Fun.empty |> Fun.add 0 1 |> Fun.add 1 0 |> Fun.add 2 2
let move_react = TBrs.parse_react "move" ~lhs:move_lhs ~rhs:move_rhs ~f_sm:(Some move_f_sm) ~f_rnm:move_f_rnm

let rules = 
  [
    deact_uav_react;
    deploy_data_react;
    downloadData_react;
    move_into_base_react;
    move_out_of_base_react;
    move_react
  ];;

let x = TBrs.explore_ss_slim ~trans_file_name:"trans.csv" ~states_file_name:"states.csv" s0 rules 300;;

