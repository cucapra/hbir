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
    | Literal l ->
        (match l with
        | XMax -> "x_max"
        | YMax -> "y_max")
    | String str -> str
    | Int i -> string_of_int i
    | Float i -> string_of_float i
    | X -> "x"
    | Y -> "y"
    | Id i -> i
    | Mem (i, d1, d2) -> i ^ "[" ^ (convert_expr d1) ^ "]" ^
        (apply_to_expr_option d2 "" (fun (d : expr) : string -> "[" ^ (convert_expr d) ^ "]"))
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
    | MemAssign ((symbol, dim_1, dim_2), expr2) -> 
        symbol ^ "[" ^ (convert_expr dim_1) ^ "]" ^
        (apply_to_expr_option dim_2 "" (fun (d : expr) : string -> "[" ^ (convert_expr d) ^ "]"))
        ^ ("= ") ^ (convert_expr expr2) ^ ";"
    | DeclAssign (str1, str2, expr) ->
        if(String.equal (convert_expr expr) "(x + (y * x_max))") then str1 ^ " " ^ str2 ^ (" = ") ^ "tile_id*csize;"
        else str1 ^ " " ^ str2 ^ (" = ") ^ (convert_expr expr) ^ ";"
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
    | [] -> ""
    | s::st -> ((convert_stmt s)  ^ "\n" ^ (convert_stmtlist st))

(* TODO: Add local tile memory *)
and convert_dmaps (dmaps : data_map list) : string =
    match dmaps with
    | [] -> "//empty dmaps list\n"
    | d::dt -> (
        match d with
        | (mt, _, i, t, (dim_x, dim_y), (_, _), (_,_), (_,_,_), _) ->
            (convert_generic t) ^ " " ^ i ^ "[" ^ (convert_expr dim_x) ^ "]" ^ 
            (* add the second dimension if it exists *)
            (apply_to_expr_option dim_y "" (fun (d : expr) : string -> "[" ^ (convert_expr d) ^ "]"))
            ^
            (
            match mt with
            | Global -> " __attribute__ ((section (\".dram\")));"
            | Local -> ";"
            )
     ) ^ ("\n" ^ (convert_dmaps dt))

and convert_target (prog : program) : string =
    match prog with
    | (t, _, _, _) ->
        (*let memsize = match d with
                      | (e, _) -> (convert_expr e) in*)
        (*TODO: Hard-code chunk size for now*)
        match t with
        | (_, (_, (_, _), _)) ->
            "int num_tiles = bsg_tiles_X * bsg_tiles_Y;\n" (*^
            "volatile int csize = " ^ memsize ^ "/(bsg_tiles_X * bsg_tiles_Y);\n"*)

and convert_data_stmt (s : data_stmt) : string =
    match s with
    | Assign (str1, expr) -> "#define " ^ str1 ^ " " ^ (convert_expr expr)

(* stmt list that shows ups in the data section, but throw define in front of everything *)
and convert_data_stmtlist (sl : data_stmt list) =
    match sl with
    | [] -> ""
    | s::st -> ((convert_data_stmt s)  ^ "\n" ^ (convert_data_stmtlist st))

(* TODO: Remove hard-coding *)
and convert_mem (prog : program) : string =
    match prog with
    | (_, _, d, _) ->
        match d with
        | (sl, dmaps) -> (convert_data_stmtlist sl) ^ "\n" ^ (convert_dmaps dmaps)

and convert_codelist (cl : code list) : string =
    match cl with
    | [] -> ""
    | (t, sl)::ct ->
        match t with
        | (_, (e1, e2)) ->
         (if (e1 == X && e2 == Y) then
            ((*TODO: replace with function call, and grab code and use as function before-hand, hard-code last tile handling chunk*)
            "if(tile_id == num_tiles-1){\n" ^
            "csize = csize + ( dim % num_tiles );\n}\n" ^
            (convert_stmtlist sl)  ^ "\n" ^ convert_codelist(ct))
         else
            (convert_codelist(ct) ^ "if(tile_id == bsg_x_y_to_id(" ^ (convert_expr e1) ^ ", " ^ (convert_expr e2) ^ ")){\n" ^
            (convert_stmtlist sl)  ^ "}\n")
         )

(* can read as foreach code section in program, add an int main() and foreach code listing? *)
let convert_ast (prog : program) : string =
    "#include \"bsg_manycore.h\"\n#include \"bsg_set_tile_x_y.h\"\n" ^
    convert_mem (prog) ^
    match prog with
    | (_, _, _, c) ->
        match c with
        | (None, cl) -> (convert_codelist cl) ^ "\n}"
        | (Some sl, cl) ->
            (convert_stmtlist sl) ^
            (convert_target prog) ^
            "int main() {\n" ^ "bsg_set_tile_x_y();\n" ^ "int tile_id = bsg_x_y_to_id(bsg_x, bsg_y);\n" ^
            (convert_codelist cl) ^ "bsg_wait_while(1);\n" ^ "\n}"

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

