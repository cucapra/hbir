open Ast
open Manycore

(* PBB add a third function here to generate host.c. will punt on makefiles for now *)
(* we want to base this off the data section so do (_,_,d,_). don't care about other 3 section and write data section to variable 'd' *)
(* We need this to do three main things besides boilerplate *)
(* 1) send kernel to tiles: hb_mc_load_binary(fd, manycore_program, &x, &y, 1); -- along with freezing and unfreezing the tiles*)
(* 2) memcpy to read variables: hammaSymbolMemcpy(fd, x, y, manycore_program, "tileDataRd", (void<star>)h_a, numBytes, hostToDevice); *)
(* 3) memcpy from write variables: hammaSymbolMemcpy(fd, x, y, manycore_program, "tileDataWr", (void<star>)h_b, numBytes, deviceToHost); *)

(*let f1_temp : tile = "", (Int 0, Int 1)*)

type bounds = expr * expr * expr * expr

(* bounds, but really want to be a *POLICY* -- symbol name -- array dimenstion *)
type memcpy = (*bounds * *)string * string

(* helpers for host and device symbol generation *)
let f1_host_symbol(symbol : string) : string = 
        "h_" ^ symbol

let f1_device_symbol(symbol : string) : string =
        (*"d_" ^ symbol*)
        symbol

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

let f1_init_device =
        "\t" ^ "uint8_t fd;\n" ^
        "\t" ^ "if (hb_mc_init_host(&fd) != HB_MC_SUCCESS) {\n" ^
        "\t\t" ^ "printf(\"failed to initialize host.\\n\");\n" ^
        "\t\t" ^ "return 0;\n" ^
        "\t" ^ "}\n\n"

(* if creating a wrapper for the hbir kernel then need to give contents of data *)
(* TODO eventually support mallocs *)
let f1_kernel_signature ( args : memcpy ) : string =
        match args with 
        | (symbol, _) ->
                "void *" ^ f1_host_symbol(symbol) ^ ", "

(* generate arbitrary data on the host side *)
let f1_host_data_gen (gen_wrapper : bool) (args : memcpy) : string = 
        match gen_wrapper with
        | true -> ""
        | false ->
        match args with 
        | (symbol, dim) ->
                let host_symbol = f1_host_symbol(symbol) in 
                "\t" ^ "int *" ^ host_symbol ^ " = (int*)malloc(" ^ dim ^ " * sizeof(int));\n" ^
                "\t" ^ "for(int i = 0; i < " ^ dim ^ "; i++) {\n" ^
                "\t\t" ^ host_symbol ^ "[i] = i;\n" ^
                "\t" ^ "}\n"

(* just do for one tile for now, but will eventually take in list of tiles *)
let f1_load_kernel (b : bounds) : string = 
        "\t" ^ "hammaLoadMultiple(fd, manycore_program, " ^ (string_of_bounds b) ^ ");\n"

(* call an operation for each tile in the bounds *)
let func_within_bounds (func : tile -> string) (b : bounds) : string =
        match b with
        | (x1, y1, x2, y2) ->
                let x : expr = String "x" in
                let y : expr = String "y" in
                let t : tile = ("", (x, y)) in 
                "\t"     ^ "for (int y = " ^ ( convert_expr y1 ) ^ "; y < " ^ ( convert_expr y2 ) ^ "; y++) {\n" ^
                "\t\t"   ^ "for (int x = " ^ ( convert_expr x1 ) ^ "; x < " ^ ( convert_expr x2 ) ^ "; x++) {\n" ^
                "\t\t"   ^ func(t) ^
                "\t\t"   ^ "}\n" ^
                "\t"     ^ "}\n"

(* do the appropriate memcpys for each variable read or write (determined by dir string) in the kernel *)
(* TODO want to give policy as well as bounds *)
let f1_memcpy (dir : string) (b : bounds) (args : memcpy) : string =
        match args with
        | (symbol, dim) ->
                (* TODO: assumes each data type is 4 bytes (32 bits) *)
                let num_bytes = dim ^ " * sizeof(int)" in
                let host_symbol = f1_host_symbol(symbol) in
                let device_symbol = f1_device_symbol(symbol) in
                let single_memcpy (t : tile) : string = 
                        "\t" ^ "hammaSymbolMemcpy(fd, " ^ string_of_tile(t) ^ " ,  manycore_program, \"" ^ 
                        device_symbol ^ "\", (void*)" ^ host_symbol ^ ", " ^ num_bytes ^ ", " ^ dir ^ ");\n"
                in
                func_within_bounds single_memcpy b


(* foreach dmap entry, do the provided function *)
let rec f1_convert_dmaps (dmaps : data_map list) (func : memcpy -> string) : string =
    match dmaps with
    | [] -> ""
    | d::dt -> (
        (* for the head, get the name (i) type (t) and xDim (dim_x) and yDim option (d_y) *)
        match d with
        | (_, _, i, _, (dim_x, d_y), (_, _), (_,_), (_,_,_), _) ->
                let dim : expr = match d_y with
                        | None -> dim_x
                        | Some dim_y -> Times (dim_x,dim_y)
                in
                let single_memcpy = i, convert_expr dim in
                func(single_memcpy) ^
                (* recursively call the function on the next element to process the whole array *)
                (f1_convert_dmaps dt func))


(* develop header for the host program, if standalone, then main, otherwise give wrapper func name *)
(*TODO need to intepret data map for mem args. also need to give a name! *)
let f1_main (gen_wrapper : bool) (dmaps : data_map list) : string = 
        let kernel_name = "hbir_kernel" in
        let binary_name = "main.riscv" in
        
        match gen_wrapper with
        | false -> (
                "int main(int argc, char *argv[]) {\n" ^
                "\t" ^ "assert(argc == 2);\n" ^
                "\t" ^ "char *manycore_program = argv[1];\n"
                )
        | true -> (     
                let sig_trail : string = f1_convert_dmaps dmaps f1_kernel_signature in
                (* remove trailing ', ' *)
                let strLen = String.length sig_trail in
                let signature = String.sub sig_trail 0 (strLen - 2) in
                "int " ^ kernel_name ^ "(" ^ signature ^ ") {\n" ^
                "\t" ^ "char *manycore_program = \"" ^ binary_name ^ "\";\n"
        )
        ^ f1_init_device

let f1_run_and_wait (b : bounds): string = 
        "\t" ^ "hammaRunMultiple(fd, " ^ string_of_bounds(b) ^ ");\n\n"


let f1_result_buffers (gen_wrapper: bool) (args : memcpy) : string = 
        match gen_wrapper with
        | true -> ""
        | false ->
        match args with 
        | (symbol, dim) ->
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
let generate_f1_host (prog : program) (gen_wrapper : bool) : string =
    f1_includes ^ 
    match prog with
    | (_, c, d, _) -> (
        let tile_bounds = f1_get_num_tiles(c) in
        match d with
        | (e, dmaps) ->
                (f1_main gen_wrapper dmaps) ^   
                (* get dmaps intended to be send in different directions *)
                let (memcpy_to_dmaps, memcpy_from_dmaps) = f1_split_dmaps(dmaps) in
                "\t" ^ "int dim = " ^ (convert_expr e) ^ ";\n" ^
                (f1_convert_dmaps memcpy_to_dmaps (f1_host_data_gen gen_wrapper)) ^
                f1_load_kernel(tile_bounds) ^ 
                (* for each data field should create a memcpy cmd*)

                let memcpy_to = (f1_memcpy "hostToDevice" tile_bounds) in
                (f1_convert_dmaps memcpy_to_dmaps memcpy_to) ^
                f1_run_and_wait(tile_bounds) ^

                (f1_convert_dmaps memcpy_from_dmaps (f1_result_buffers gen_wrapper)) ^ 
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