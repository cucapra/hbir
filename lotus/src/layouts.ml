(* everything having to do with memory layouts and  *)

type mem_type =
    | Global
    | Local

type mem_location =
    | Host
    | Device

(* data distribution policy *)
type dist_policy = 
    | Chunked
    | Strided
    (* if custom than allow code to be written there {} *)
    | Custom

(* memory layout that multiple data maps can share *)
(* mem-type (global/local) ~ hostToDevice or deviceToHost ~ symbol name ~ 
   [x] ~ [y] ~ *)
(* layout name ~ physical storage (TODO: default to global no coords) ~ distribution ~ transfer type*)
type data_layout = string * mem_type * dist_policy

(* save the data layouts for lookup later *)
let data_layout_table = "cool story bro"

(* TODO: don't cheat here *)
let rec find_data_layout_by_symbol (name : string) (layouts : data_layout list) : data_layout =
    match layouts with
    | [] -> (name,Global, Chunked)
    | _::lt -> (find_data_layout_by_symbol name lt)