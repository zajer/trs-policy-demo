open Policy
module SetOfActions = Set.Make(Action);;
module Demo =
	struct

	type i = (int*int)
	type t = Unreachable | Reachable of i*SetOfActions.t
	type k = t list
	type f = t -> int -> t

let set_of_actions_2_string set =
	let part1 = SetOfActions.fold (fun el sum -> (el.label^"^"^ string_of_int el.step ^"->")^sum ) set ""
	and part2 = "END" in
	"{"^part1 ^ part2^"}"

let i_to_string (s:i) =
	match s with
	|((a0,x0)) ->
	"[" ^ string_of_int a0 ^ ":" ^ string_of_int x0 ^ "]"
let k_to_string ks =
	List.map 
        (
                fun c -> 
                        match c with
                        | Unreachable -> ""
                        | Reachable (((a0,x0)),set) -> ( i_to_string ((a0,x0)) ^ set_of_actions_2_string set )
        ) 
        ks
        |> String.concat ""
end
open Demo
let f_null _ _ = Unreachable
let f0 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="0"; step=t+1} set in
			Reachable (new_state, new_set)
let f1 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="1"; step=t+1} set in
			Reachable (new_state, new_set)
let f2 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="2"; step=t+1} set in
			Reachable (new_state, new_set)
let f3 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="3"; step=t+1} set in
			Reachable (new_state, new_set)
let f4 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="4"; step=t+1} set in
			Reachable (new_state, new_set)
let f5 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="5"; step=t+1} set in
			Reachable (new_state, new_set)
let f6 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="6"; step=t+1} set in
			Reachable (new_state, new_set)
let f7 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="7"; step=t+1} set in
			Reachable (new_state, new_set)
let f8 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="8"; step=t+1} set in
			Reachable (new_state, new_set)
let f9 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="9"; step=t+1} set in
			Reachable (new_state, new_set)
let f10 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="10"; step=t+1} set in
			Reachable (new_state, new_set)
let f11 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="11"; step=t+1} set in
			Reachable (new_state, new_set)
let f12 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="12"; step=t+1} set in
			Reachable (new_state, new_set)
let f13 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="13"; step=t+1} set in
			Reachable (new_state, new_set)
let f14 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="14"; step=t+1} set in
			Reachable (new_state, new_set)
let f15 e t =
	match e with
	| Unreachable -> Unreachable
	| Reachable (((a0,x0)),set) ->
		let new_state = ((a0,x0+1))
		and new_set = SetOfActions.add {label="15"; step=t+1} set in
			Reachable (new_state, new_set)
