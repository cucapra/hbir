
open Ast
(*
open Manycore
open Layouts

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
        (expr_emit x1) ^ ", " ^ (expr_emit y1) ^ ", " ^ 
        (expr_emit x2) ^ ", " ^ (expr_emit y2)

(* helper to convert tile coords to a string *)
let string_of_tile(t: tile_decl) : string = 
  if List.length t.tile_dims >= 2
    then (expr_emit (List.nth t.tile_dims 0)) ^ ", " ^ (expr_emit (List.nth t.tile_dims 1))
    else ""

(* define some boilerplate strings *)
let f1_includes (gen_wrapper: bool) : string = 
        (
        match gen_wrapper with
        | true -> "#include \"wrapper.h\"\n" 
        | false -> ""
        )
        ^
        "#include \"f1_helper.h\"\n"

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
                "\t" ^ "int *" ^ host_symbol ^ " = (int* )malloc(" ^ dim ^ " * sizeof(int));\n" ^
                "\t" ^ "for(int i = 0; i < " ^ dim ^ "; i++) {\n" ^
                "\t\t" ^ host_symbol ^ "[i] = i;\n" ^
                "\t" ^ "}\n"

(* just do for one tile for now, but will eventually take in list of tiles *)
let f1_load_kernel (b : bounds) : string = 
        "\t" ^ "hammaLoadMultiple(fd, manycore_program, " ^ (string_of_bounds b) ^ ");\n"

(* call an operation for each tile in the bounds *)
let func_within_bounds (func : tile_decl -> string) (b : bounds) : string =
        match b with
        | (x1, y1, x2, y2) ->
                let t : tile_decl = {tile_name="what's this for?"; tile_dims=[]; mem_decls=[]} in 
                "\t"     ^ "for (int y = " ^ ( expr_emit y1 ) ^ "; y < " ^ ( expr_emit y2 ) ^ "; y++) {\n" ^
                "\t\t"   ^ "for (int x = " ^ ( expr_emit x1 ) ^ "; x < " ^ ( expr_emit x2 ) ^ "; x++) {\n" ^
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
                let single_memcpy (t : tile_decl) : string = 
                        "\t" ^ "hammaSymbolMemcpy(fd, " ^ string_of_tile(t) ^ " ,  manycore_program, \"" ^ 
                        device_symbol ^ "\", (void* )" ^ host_symbol ^ ", " ^ num_bytes ^ ", " ^ dir ^ ");\n"
                in
                func_within_bounds single_memcpy b


(* foreach dmap entry, do the provided function *)
let rec f1_convert_dmaps (dmaps : data_decl list) (func : memcpy -> string) : string =
    match dmaps with
    | [] -> ""
    | d::dt -> (
      (* for the head, get the name (i) type (t) and xDim (dim_x) and yDim option (d_y) *)
      let dim : expr = match (d.data_dims) with
              | [dim_x] -> dim_x
              | [dim_x; dim_y] -> Times (dim_x,dim_y)
              | _ -> failwith "only 1 or 2 dimensional data supported."
      in
      let single_memcpy = d.data_name, expr_emit dim in
      func(single_memcpy) ^
      (* recursively call the function on the next element to process the whole array *)
      (f1_convert_dmaps dt func))


(* create the whole signature (all arguments) for hbir kernel *)
let f1_create_full_signature (dmaps: data_decl list) : string =
        let sig_trail : string = f1_convert_dmaps dmaps f1_kernel_signature in
        (* remove trailing ', ' *)
        let strLen = String.length sig_trail in
        let signature = String.sub sig_trail 0 (strLen - 2) in
        signature

let f1_create_kernel_header (kernel_name : string) (dmaps : data_decl list) : string =
        let signature = (f1_create_full_signature dmaps) in
        "int " ^ kernel_name ^ "(" ^ signature ^ ")"

(* develop header for the host program, if standalone, then main, otherwise give wrapper func name *)
(*TODO need to intepret data map for mem args. also need to give a name! *)
let f1_main (gen_wrapper : bool) (dmaps : data_decl list) : string = 
        let kernel_name = "hbir_kernel" in
        let binary_name = "main.riscv" in
        
        match gen_wrapper with
        | false -> (
                "int main(int argc, char *argv[]) {\n" ^
                "\t" ^ "assert(argc == 2);\n" ^
                "\t" ^ "char *manycore_program = argv[1];\n"
                )
        | true -> (     
                (f1_create_kernel_header kernel_name dmaps) ^ " {\n" ^
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
                "\t" ^ "int *" ^ host_symbol ^ " = (int* )malloc(" ^ dim ^ " * sizeof(int));\n"

let f1_cleanup_host : string = "\t// cleanup host\n\treturn 0;\n}\n"


(* https://stackoverflow.com/questions/26484498/ocaml-splitting-list-into-two-separate-greater-or-lesser-than-given-number *)
(* split data entry list into two lists, one's that need host to device memcpy and others device to host  *)
let f1_split_dmaps (dmaps : data_decl list) =
        let rec split((dmaps : data_decl list), (dmaps_to : data_decl list), (dmaps_from : data_decl list)) =
                match dmaps with
                (* when your initial list is empty, you have to return the accumulators *)
                | [] -> (dmaps_to, dmaps_from)
                | d::dt ->
                        (* break open dmap to get the mem send direction *)
                        (* append dmap entry to either memcpy_to or memcpy_from dmaps *)
                        match d.data_dir with
                        | In -> split ( dt, d::dmaps_to, dmaps_from ) 
                        | Out -> split( dt, dmaps_to, d::dmaps_from )
        in 
        (* start recursion *)
        split ( dmaps, [], [] )

(* figure out how many tiles should be allocated for a particular kernel *)
let f1_get_num_tiles ( _: config_decl ) : bounds = (Int 0, Int 0, Int 0, Int 0)

*)
(* emits the host code *)
let generate_f1_host (_: program) (_: bool) : string = ""
(*
  (f1_includes gen_wrapper) ^
  match prog with
  | (_, config, data, _) -> (
    let tile_bounds = f1_get_num_tiles(config) in
      (f1_main gen_wrapper data.data_decls) ^   

      (* get dmaps intended to be send in different directions *)
      let (memcpy_to_dmaps, memcpy_from_dmaps) = f1_split_dmaps(data.data_decls) in
      let memcpy_to = (f1_memcpy "hostToDevice" tile_bounds) in
      let memcpy_from = (f1_memcpy "deviceToHost" tile_bounds) in
        (convert_data_stmtlist data.constant_decls) ^
        (*"\t" ^ "int dim = " ^ (expr_emit e) ^ ";\n" ^*)
        (f1_convert_dmaps memcpy_to_dmaps (f1_host_data_gen gen_wrapper)) ^
        f1_load_kernel(tile_bounds) ^ 

        (* for each data field should create a memcpy cmd*)
        (f1_convert_dmaps memcpy_to_dmaps memcpy_to) ^
        f1_run_and_wait(tile_bounds) ^
        (f1_convert_dmaps memcpy_from_dmaps (f1_result_buffers gen_wrapper)) ^ 
        (f1_convert_dmaps memcpy_from_dmaps memcpy_from) ^
        f1_cleanup_host
  )
*)
(* generates header file to include in your program to run the hbir kernel *)
let generate_f1_wrapper_header (_: program) : string = ""
(*
    match prog with
    | (_, _, d, _) -> 
      let kernel_name = "hbir_kernel" in
      let unique_def_name = "___" ^ kernel_name ^ "___" in
      "#ifndef " ^ unique_def_name ^ "\n" ^
      "#define " ^ unique_def_name ^ "\n" ^
      (f1_create_kernel_header kernel_name d.data_decls) ^ ";\n" ^
      "#endif\n"
*)

(* can read as foreach code section in program, add an int main() and foreach code listing? *)
let generate_f1_device (_ : program) : string = ""
  (*
    "#include \"bsg_manycore.h\"\n#include \"bsg_set_tile_x_y.h\"\n" ^
    convert_mem (prog) ^
    match prog with
    | (_, _, _, c) -> "int main() {\n" ^ "bsg_set_tile_x_y();\n" ^ "int tile_id = bsg_x_y_to_id(bsg_x, bsg_y);\n" ^ (convert_target prog) ^
        match c with
        | (None, cl) -> (convert_codelist cl) ^ "\n}"
        | (Some sl, cl) ->
            (convert_stmtlist sl) ^ (convert_codelist cl) ^ "\n}"
*)
let generate_f1_makefile (_: program) : string = ""
  (*
    match prog.target_section with
    | [] -> ""
    | GlobalMemDecl _ :: _ -> ""
    | TileDecl tile :: _ ->
        (
          if List.length tile.tile_dims >= 2
            then
              "bsg_tiles_X = " ^ (expr_emit (List.nth tile.tile_dims 0)) ^
              "\nbsg_tiles_Y = " ^ (expr_emit (List.nth tile.tile_dims 1))
            else ""
        ) ^
        makefile ^ "\n"
        *)
(* any preprocessing before actually doing the compilation, like constructing the symbol table or static analysis? *)
let preprocessing_phase (_: program) = ()
(*
    let ds = prog.data_section.ds_data_decls in
        if List.length ds > 0
          then (generate_layout_symbol_table (List.nth ds 0))
          else ()
*)
