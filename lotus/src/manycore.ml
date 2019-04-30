open Ast

(* BSG Manycore backend to target Manycore RTL Simulator and F1 Instance *)
(* Translates AST to C-like code and generates Makefile to run on Manycore *)

let makefile = "all: main.run\n
proc_exe_logs: X0_Y0.pelog X1_Y0.pelog\n
OBJECT_FILES=main.o bsg_set_tile_x_y.o bsg_printf.o\n
include ../Makefile.include\n
BSG_FPU_OP=0\n
main.riscv: $(OBJECT_FILES) $(SPMD_COMMON_OBJECTS) ../common/crt.o
\t$(RISCV_LINK) $(OBJECT_FILES) $(SPMD_COMMON_OBJECTS) -o $@ $(RISCV_LINK_OPTS)\n
main.o: Makefile\n
include ../../mk/Makefile.tail_rules"

let convert_generic (g : generic_type) : string =
    match g with
    | BoolTyp -> "boolean"
    | IntTyp -> "int"
    | FloatTyp -> "float"

let rec convert_expr (e : expr) : string =
    match e with
    (*TODO: Need to fix this *)
    | Literal _ -> "literal"
    | String str -> str
    | Int i -> string_of_int i
    | Id i -> i
    | Mem (i, e) -> i ^ "[" ^ (convert_expr e) ^ "]"
    | Plus (e1, e2) -> "("^(convert_expr e1) ^ " + " ^ (convert_expr e2)^")"
    | Minus (e1, e2) -> "("^(convert_expr e1) ^ " - " ^ (convert_expr e2)^")"
    | Times (e1, e2) -> "("^(convert_expr e1) ^ " * " ^ (convert_expr e2)^")"
    | Div (e1, e2) -> "("^(convert_expr e1) ^ " / " ^ (convert_expr e2)^")"
    | Bool b -> (match b with
                | true -> "1"
                | false -> "0")
    | Eq (e1, e2) -> "("^(convert_expr e1) ^ " == " ^ (convert_expr e2)^")"
    | Neq (e1, e2) -> "("^(convert_expr e1) ^ " != " ^ (convert_expr e2)^")"
    | Lt (e1, e2) -> "("^(convert_expr e1) ^ " < " ^ (convert_expr e2)^")"
    | Gt (e1, e2) -> "("^(convert_expr e1) ^ " > " ^ (convert_expr e2)^")"
    | Lteq (e1, e2) -> "("^(convert_expr e1) ^ " <= " ^ (convert_expr e2)^")"
    | Gteq (e1, e2) -> "("^(convert_expr e1) ^ " >= " ^ (convert_expr e2)^")"
    | And (e1, e2) -> "("^(convert_expr e1) ^ " && " ^ (convert_expr e2)^")"
    | Or (e1, e2) -> "("^(convert_expr e1) ^ " || " ^ (convert_expr e2)^")"

let rec convert_iblist (il : if_block list) : string =
    match il with
    | [] -> ""
    | i::it -> "else " ^ (convert_ib i) ^ (convert_iblist it)

and convert_stmt (s : stmt) : string =
    match s with
    | Decl (str1, str2) -> str1 ^ " " ^ str2 ^ ";"
    | Assign (str1, expr) -> str1 ^ (" = ") ^ (convert_expr expr) ^ ";"
    | MemAssign ((str1, expr1), expr2) -> str1 ^ "[" ^ (convert_expr expr1) ^ "]" ^ ("= ") ^ (convert_expr expr2) ^ ";"
    | DeclAssign (str1, str2, expr) -> str1 ^ " " ^ str2 ^ (" = ") ^ (convert_expr expr) ^ ";"
    | If (i,il,s) -> (
        match s with
        | None -> ((convert_ib i) ^ (convert_iblist il))
        | Some st -> ((convert_ib i) ^ (convert_iblist il) ^ "else {\n" ^ (convert_stmtlist st) ^ "}\n")
        )
    | While (e,sl) -> "while ( " ^ (convert_expr e) ^ " ) {\n" ^ (convert_stmtlist sl) ^ "}\n"
    | For ((s1,e1,(i,e2)),sl) -> "for (" ^ (convert_stmt s1) ^ " " ^ (convert_expr e1) ^ "; " ^ i ^ "=" ^ (convert_expr e2) ^ ") {\n" ^
                            (convert_stmtlist sl) ^ "}\n"
    | Break _ -> "break "
    | Print s -> "bsg_printf(" ^ s ^ ");\n"
    | BsgFinish -> "bsg_finish();\n"

and convert_ib (i : if_block) : string =
    match i with
    | (e,sl) -> "if ( " ^ (convert_expr e) ^ " ) {\n" ^ (convert_stmtlist sl) ^ "} "

and convert_stmtlist (sl : stmt list) : string =
    match sl with
    | [] -> "//empty stmt list\n"
    | s::st -> ((convert_stmt s)  ^ "\n" ^ (convert_stmtlist st))

(* TODO: Add local tile memory *)
and convert_dmaps (dmaps : data_map list) : string =
    match dmaps with
    | [] -> "//empty dmaps list\n"
    | d::dt -> (
        match d with
        | (i, t, (dim1, _), (_, _), (_,_), (_,_,_), _) -> (convert_generic t) ^ " " ^ i ^ "[" ^ (convert_expr dim1) ^ "]" ^
        " __attribute__ ((section (\".dram\")));"
     ) ^ ("\n" ^ (convert_dmaps dt))

(* TODO: Remove hard-coding *)
and convert_mem (prog : program) : string =
    match prog with
    | (_, _, d, _) ->
        match d with
        | (e, dmaps) -> "int dim = " ^ (convert_expr e) ^ ";\n" ^ (convert_dmaps dmaps)

and convert_codelist (cl : code list) : string =
    match cl with
    | [] -> "//empty code list\n"
    | (_,sl)::ct -> (convert_stmtlist sl)  ^ "\n" ^ convert_codelist(ct)

(* can read as foreach code section in program, add an int main() and foreach code listing? *)
let convert_ast (prog : program) : string =
    "#include \"bsg_manycore.h\"\n#include \"bsg_set_tile_x_y.h\"\n" ^
    convert_mem (prog) ^
    match prog with
    | (_, _, _, c) -> "int main() {\n" ^ "bsg_set_tile_x_y();\n" ^
        match c with
        | (None, cl) -> (convert_codelist cl) ^ "\n}"
        | (Some sl, cl) ->
            (convert_stmtlist sl) ^ (convert_codelist cl) ^ "\n}"

let generate_makefile (prog : program) : string =
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


(* PBB add a third function here to generate host.c. will punt on makefiles for now *)
(* we want to base this off the data section so do (_,_,d,_). don't care about other 3 section and write data section to variable 'd' *)
(* We need this to do three main things besides boilerplate *)
(* 1) send kernel to tiles: hb_mc_load_binary(fd, manycore_program, &x, &y, 1); -- along with freezing and unfreezing the tiles*)
(* 2) memcpy to read variables: hammaSymbolMemcpy(fd, x, y, manycore_program, "tileDataRd", (void<star>)h_a, numBytes, hostToDevice); *)
(* 3) memcpy from write variables: hammaSymbolMemcpy(fd, x, y, manycore_program, "tileDataWr", (void<star>)h_b, numBytes, deviceToHost); *)

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
        "\t" ^ "char *manycore_program = argv[1];" ^
        "\t" ^ "uint8_t fd;\n" ^
        "\t" ^ "if (hb_mc_init_host(&fd) != HB_MC_SUCCESS) {\n" ^
        "\t\t" ^ "printf(\"failed to initialize host.\\n\");\n" ^
        "\t\t" ^ "return 0;\n" ^
        "\t" ^ "}\n\n"

let f1_host_data_gen = "// host data gen here\n"

(* just do for one tile for now, but will eventually take in list of tiles *)
let f1_load_kernel (t : tile) : string = 
        match t with
        | (_, (x, y)) ->
        "\t" ^ "uint8_t x = " ^ (convert_expr x) ^ ", y = " ^ (convert_expr y) ^ ";\n" ^
        "\t" ^ "hb_mc_freeze(fd, " ^ string_of_tile(t) ^ ");\n" ^
        "\t" ^ "hb_mc_set_tile_group_origin(fd, 0, 1, 0, 1);\n" ^
        "\t" ^ "hb_mc_load_binary(fd, manycore_program, &x, &y, 1);\n\n"

(* tile (x,y coords) -- symbol name -- array dimenstion (could be '5*6' so use expr type instead of int) *)
type memcpy = tile * string * expr

(* helpers for host and device symbol generation *)
let f1_host_symbol(symbol : string) : string = 
        "h_" ^ symbol

let f1_device_symbol(symbol : string) : string =
        (*"d_" ^ symbol*)
        symbol

(* do the appropriate memcpys for each variable read in the kernel *)
let f1_host_to_device(args : memcpy) : string = 
        match args with
        | (t, symbol, dim) ->
                (* TODO: assumes each data type is 4 bytes (32 bits) *)
                let num_bytes = string_of_int(int_of_string((convert_expr dim)) * 4) in
                let host_symbol = f1_host_symbol(symbol) in
                let device_symbol = f1_device_symbol(symbol) in
                "\t" ^ "hammaSymbolMemcpy(fd, " ^ string_of_tile(t) ^ ", manycore_program, \"" ^ 
                device_symbol ^ "\", (void*)" ^ host_symbol ^ ", " ^ num_bytes ^ ", hostToDevice);\n"

let f1_run_and_wait (t : tile) : string = 
        "\t" ^ "hb_mc_unfreeze(fd, " ^ string_of_tile(t) ^ ");\n" ^
        "\t" ^ "waitForKernel(fd);\n\n"

let f1_device_to_host : string = "\t" ^ "hammaSymbolMemcpy(fd, x, y, manycore_program, \"d_b\", (void*)h_b, numBytes, deviceToHost);\n"

let f1_cleanup_host : string = "\t// cleanup host\n\treturn 0;\n}\n"

(* need to cast int to tuple (which has an Int field of type int) *)
let f1_temp : tile = "", (Int 0, Int 1)

(* emits the host code *)
let generate_f1_host (prog : program) : string =
    f1_includes ^
    f1_main ^    
    match prog with
    | (_, _, d, _) -> 
        f1_host_data_gen ^
        f1_load_kernel(f1_temp) ^ 
        f1_host_to_device(f1_temp, "A", Int 4) ^
        f1_run_and_wait(f1_temp) ^
        f1_device_to_host ^
        f1_cleanup_host ^
        match d with
        | (e, dmaps) -> "int dim = " ^ (convert_expr e) ^ ";\n" ^ (convert_dmaps dmaps)
