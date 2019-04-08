open Ast

(* BSG Manycore backend to target Manycore RTL Simulator and F1 Instance *)
(* Translates AST to C-like code and generates Makefile to run on Manycore *)

let makefile = "all: main.run\n
proc_exe_logs: X0_Y0.pelog X1_Y0.pelog\n
OBJECT_FILES=main.o bsg_set_tile_x_y.o bsg_printf.o\n
include ../Makefile.include\n
BSG_FPU_OP=0\n
main.riscv: $(OBJECT_FILES) $(SPMD_COMMON_OBJECTS) ../common/crt.o
        $(RISCV_LINK) $(OBJECT_FILES) $(SPMD_COMMON_OBJECTS) -o $@ $(RISCV_LINK_OPTS)\n
main.o: Makefile\n
include ../../mk/Makefile.tail_rules"

let rec convert_expr (e : expr) : string =
    match e with
    | String str -> str
    | Int i -> string_of_int i
    | Id i -> i
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

and convert_ib (i : if_block) : string =
    match i with
    | (e,sl) -> "if ( " ^ (convert_expr e) ^ " ) {\n" ^ (convert_stmtlist sl) ^ "} "

and convert_stmtlist (sl : stmt list) : string =
    match sl with
    | [] -> "//empty stmt list\n"
    | s::st -> ((convert_stmt s)  ^ "\n" ^ (convert_stmtlist st))

let convert_ast (prog : program) : string =
    "#include \"bsg_manycore.h\"\n#include \"bsg_set_tile_x_y.h\"\n" ^
    match prog with
        | (_, _, _, c) -> "int main() {\n" ^
            match c with
            | [] -> "//empty code list\n}\n"
            | ch::_ -> match ch with
                | (_, sl) -> (convert_stmtlist sl) ^ "bsg_finish();\n}\n"

let generate_makefile (prog : program) : string =
    match prog with
        | (target, _, _, _) ->
            match target with
                    | (m1, t) -> match m1 with (_, _, (_, _)) -> "" ^
                        match t with (_, (e2, e3), m2) -> "bsg_tiles_X = " ^ (convert_expr e2) ^
                                                           "\nbsg_tiles_Y = " ^ (convert_expr e3) ^ "\n" ^
                                                           makefile ^
                            match m2 with (_, _, (_, _)) ->
                                ""
