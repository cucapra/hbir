open Ast
open Manycore

(* PBB add a third function here to generate host.c. will punt on makefiles for now *)
(* we want to base this off the data section so do (_,_,d,_). don't care about other 3 section and write data section to variable 'd' *)
(* We need this to do three main things besides boilerplate *)
(* 1) send kernel to tiles: hb_mc_load_binary(fd, manycore_program, &x, &y, 1); -- along with freezing and unfreezing the tiles*)
(* 2) memcpy to read variables: hammaSymbolMemcpy(fd, x, y, manycore_program, "tileDataRd", (void<star>)h_a, numBytes, hostToDevice); *)
(* 3) memcpy from write variables: hammaSymbolMemcpy(fd, x, y, manycore_program, "tileDataWr", (void<star>)h_b, numBytes, deviceToHost); *)

let f1_temp : tile = "", (Int 0, Int 1)

type bounds = expr * expr * expr * expr

let string_of_bounds(b : bounds) : string =
        match b with
        | (x1, y1, x2, y2) ->
        (convert_expr x1) ^ ", " ^ (convert_expr y1) ^ ", " ^ 
        (convert_expr x2) ^ ", " ^ (convert_expr y2)

(* helper to convert tile coords to a string *)
let string_of_tile(t: tile) : string = 
        match t with
        | (_, (x, y)) ->
        (convert_expr x) ^ ", " ^ (convert_expr y)

(* define some boilerplate strings *)
let f1_includes = "#include \"f1_helper.h\"\n"
let f1_main = 
        "int main(int argc, char *argv[]) {\n" ^
        "\t" ^ "assert(argc == 2);\n" ^
        "\t" ^ "char *manycore_program = argv[1];\n" ^
        "\t" ^ "uint8_t fd;\n" ^
        "\t" ^ "if (hb_mc_init_host(&fd) != HB_MC_SUCCESS) {\n" ^
        "\t\t" ^ "printf(\"failed to initialize host.\\n\");\n" ^
        "\t\t" ^ "return 0;\n" ^
        "\t" ^ "}\n\n"

(* helpers for host and device symbol generation *)
let f1_host_symbol(symbol : string) : string = 
        "h_" ^ symbol

let f1_device_symbol(symbol : string) : string =
        (*"d_" ^ symbol*)
        symbol

(* tile (x,y coords) -- symbol name -- array dimenstion *)
type memcpy = tile * string * string

(* generate arbitrary data on the host side *)
let f1_host_data_gen(args : memcpy) : string = 
        match args with 
        | (_, symbol, dim) ->
                let host_symbol = f1_host_symbol(symbol) in 
                "\t" ^ "int *" ^ host_symbol ^ " = (int*)malloc(" ^ dim ^ " * sizeof(int));\n" ^
                "\t" ^ "for(int i = 0; i < " ^ dim ^ "; i++) {\n" ^
                "\t\t" ^ host_symbol ^ "[i] = i;\n" ^
                "\t" ^ "}\n"

(* just do for one tile for now, but will eventually take in list of tiles *)
let f1_load_kernel (b : bounds) : string = 
        "\t" ^ "hammaLoadMultiple(fd, manycore_program, " ^ (string_of_bounds b) ^ ");\n"


(* do the appropriate memcpys for each variable read or write (determined by dir string) in the kernel *)
(* TODO probably don't want to give bounds *)
let f1_memcpy (dir : string) (b : bounds) (args : memcpy) : string =
        match args with
        | (_, symbol, dim) ->
                (* TODO: assumes each data type is 4 bytes (32 bits) *)
                let num_bytes = dim ^ " * sizeof(int)" in
                let host_symbol = f1_host_symbol(symbol) in
                let device_symbol = f1_device_symbol(symbol) in
                "\t" ^ "hammaSymbolMemcpy(fd, " ^ string_of_bounds(b) ^ ",  manycore_program, \"" ^ 
                device_symbol ^ "\", (void*)" ^ host_symbol ^ ", " ^ num_bytes ^ ", " ^ dir ^ ");\n"

(* uhhh... foreach element in dmap array create the appropriate memcpy (or even host gen) *)
let rec f1_convert_dmaps (dmaps : data_map list) (func : memcpy -> string) : string =
    match dmaps with
    | [] -> "//empty dmaps list\n"
    | d::dt -> (
        (* for the head, get the name (i) type (t) and xDim (dim1) *)
        match d with
        | (_, _, i, _, (dim1, _), (_, _), (_,_), (_,_,_), _) ->
                let single_memcpy = f1_temp, i, convert_expr dim1 in
                func(single_memcpy) ^
                (* recursively call the function on the next element to process the whole array *)
                (f1_convert_dmaps dt func))


let f1_run_and_wait (b : bounds): string = 
        "\t" ^ "hammaRunMultiple(fd, " ^ string_of_bounds(b) ^ ");\n\n"


let f1_result_buffers (args : memcpy) : string = 
        match args with 
        | (_, symbol, dim) ->
                let host_symbol = f1_host_symbol(symbol) in 
                "\t" ^ "int *" ^ host_symbol ^ " = (int*)malloc(" ^ dim ^ " * sizeof(int));\n"

let f1_cleanup_host : string = "\t// cleanup host\n\treturn 0;\n}\n"


(* https://stackoverflow.com/questions/26484498/ocaml-splitting-list-into-two-separate-greater-or-lesser-than-given-number *)
(* split data entry list into two lists, one's that need host to device memcpy and others device to host  *)
let f1_split_dmaps (dmaps : data_map list) =
        let rec split((dmaps : data_map list), (dmaps_to : data_map list), (dmaps_from : data_map list)) =
                match dmaps with
                (* when your initial list is empty, you have to return the accumulators *)
                | [] -> (dmaps_to, dmaps_from)
                | d::dt ->
                        (* break open dmap to get the mem send direction *)
                        match d with
                        | (_, dir, _, _, (_, _), (_, _), (_,_), (_,_,_), _) -> 
                                (* append dmap entry to either memcpy_to or memcpy_from dmaps *)
                                match dir with
                                | Host -> split ( dt, d::dmaps_to, dmaps_from ) 
                                | Device -> split( dt, dmaps_to, d::dmaps_from )
        in 
        (* start recursion *)
        split ( dmaps, [], [] )

(* figure out how many tiles should be allocated for a particular kernel *)
let f1_get_num_tiles ( c : config_decl ) : bounds =
        match c with
        | (g) ->
                let rec traverse_configs (( groups : group_decl list ), ( bnds : bounds list )) =
                        match groups with
                        | [] -> bnds
                        | b::bt ->
                                match b with 
                                (* TODO recursion into the group, not supported for now *)
                                | NestedGroup (_, (_ , _) , _) -> bnds
                                (* extract dimension of the group *)
                                | GroupStmt (_, (w , h) , _) ->
                                        (* add configs to the list *)
                                        let new_bounds = (Int 0, Int 1, w, Plus(h, Int 1)) in
                                        let new_blist = new_bounds::bnds in
                                        (* TODO recursion for next config in the list *)
                                        traverse_configs(bt, new_blist)
                (* get the dimensions as a string for now *)
                in
                let bnds = traverse_configs(g, []) in
                (* TODO just return the first one *)
                match bnds with
                | [] -> (Int 0, Int 0, Int 0, Int 0)
                | b::_ ->
                      b  

(* emits the host code *)
let generate_f1_host (prog : program) : string =
    f1_includes ^
    f1_main ^    
    match prog with
    | (_, c, d, _) -> (
        let tile_bounds = f1_get_num_tiles(c) in
        match d with
        | (e, dmaps) ->
                (* get dmaps intended to be send in different directions *)
                let (memcpy_to_dmaps, memcpy_from_dmaps) = f1_split_dmaps(dmaps) in
                "\t" ^ "int dim = " ^ (convert_expr e) ^ ";\n" ^
                (f1_convert_dmaps memcpy_to_dmaps f1_host_data_gen) ^
                f1_load_kernel(tile_bounds) ^ 
                (* for each data field should create a memcpy cmd*)

                let memcpy_to = (f1_memcpy "hostToDevice" tile_bounds) in
                (f1_convert_dmaps memcpy_to_dmaps memcpy_to) ^
                f1_run_and_wait(tile_bounds) ^

                (f1_convert_dmaps memcpy_from_dmaps f1_result_buffers) ^ 
                let memcpy_from = (f1_memcpy "deviceToHost" tile_bounds) in
                (f1_convert_dmaps memcpy_from_dmaps memcpy_from) ^
                f1_cleanup_host
    )
(* can read as foreach code section in program, add an int main() and foreach code listing? *)
let generate_f1_device (prog : program) : string =
    "#include \"bsg_manycore.h\"\n#include \"bsg_set_tile_x_y.h\"\n" ^
    convert_mem (prog) ^
    match prog with
    | (_, _, _, c) -> "int main() {\n" ^ "bsg_set_tile_x_y();\n" ^ "int tile_id = bsg_x_y_to_id(bsg_x, bsg_y);\n" ^
        match c with
        | (None, cl) -> (convert_codelist cl) ^ "\n}"
        | (Some sl, cl) ->
            (convert_stmtlist sl) ^ (convert_codelist cl) ^ "\n}"

let generate_f1_makefile (prog : program) : string =
    match prog with
    | (target, _, _, _) ->
        match target with
                | (m1, t) -> match m1 with (_, _, (_, _)) -> "" ^
                    match t with (_, (e2, e3), m2) -> "bsg_tiles_X = " ^ (convert_expr e2) ^
                                                       "\nbsg_tiles_Y = " ^ (convert_expr e3) ^ "\n" ^
                                                       makefile ^
                        match m2 with
                            | None -> ""
                            | Some mem -> match mem with (_, _, (_, _)) -> ""