open Ast
(*open Layouts *)

(* BSG Manycore backend to target Manycore RTL Simulator and F1 Instance *)
(* Translates AST to C-like code and generates Makefile to run on Manycore *)

let (#%) = Printf.sprintf

let makefile = "all: main.run\n
proc_exe_logs: X0_Y0.pelog X1_Y0.pelog\n
OBJECT_FILES=main.o bsg_set_tile_x_y.o bsg_printf.o\n
include ../Makefile.include\n
BSG_FPU_OP=0\n
main.riscv: $(OBJECT_FILES) $(SPMD_COMMON_OBJECTS) ../common/crt.o
\t$(RISCV_LINK) $(OBJECT_FILES) $(SPMD_COMMON_OBJECTS) -o $@ $(RISCV_LINK_OPTS)\n
main.o: Makefile\n
include ../../mk/Makefile.tail_rules"

let typ_emit (g : typ) : string =
    match g with
    | BoolTyp -> "boolean"
    | IntTyp -> "int"
    | FloatTyp -> "float"

let rec expr_emit (e : expr) : string =
  let emit_from_binop (binop : binop) : string =
    match binop with
        | Plus -> "+"
        | Minus -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Eq -> "=="
        | Neq -> "!="
        | Lt -> "<"
        | Gt -> ">"
        | Lteq -> "<="
        | Gteq -> ">="
        | And -> "&&"
        | Or -> "||" in

    match e with
    | VarExpr str -> str
    | IntExpr v -> string_of_int v
    | FloatExpr v -> string_of_float v
    | BoolExpr b -> if b then "true" else "false"
    | DerefExpr (e, es) -> 
        let dims = 
          List.fold_left (fun s e -> "%s[%s]" #% s (expr_emit e)) "" es in
          (expr_emit e) ^ dims
    | BinAppExpr (binop, e1, e2) -> 
        "%s %s %s" #% (expr_emit e1) (emit_from_binop binop) (expr_emit e2)

(*
(* synthesize an access pattern based on the user specified data layout *)
and convert_inferred_iter (iter : inferred_iterator): string =
    match iter with
    | (iterName, dim, layoutName, x, y) -> (
        let dlyt : old_data_layout = find_data_layout_by_symbol layoutName in
        match dlyt with
            | (_, _, dist_policy) ->
                match dist_policy with
                | Chunked -> (
                    let chunk_size_iter = "chunked_size_" ^ iterName in
                    let chunk_start_iter = "chunked_mem_start_" ^ iterName in
                    "int " ^ chunk_size_iter ^ " = " ^ (expr_emit dim) ^ "/(bsg_tiles_X * bsg_tiles_Y);\n" ^
                    
                    (* last one should do a lil' more *)
                    "if ((" ^ (expr_emit x) ^ " + " ^ (expr_emit y) ^ " * bsg_tiles_X) == num_tiles - 1) {\n" ^
                    chunk_size_iter ^ " += " ^ (expr_emit dim) ^ " % num_tiles;\n" ^
                    "}\n" ^


                    "int " ^ chunk_start_iter ^ " = (" ^ (expr_emit x) ^ " * " ^
                    (expr_emit y) ^ " * " ^ "bsg_tiles_X" ^ ") * " ^ chunk_size_iter ^ ";\n" ^

                    "for (int " ^ iterName ^ " = " ^ chunk_start_iter ^ "; " ^ iterName ^ 
                    " < " ^ chunk_start_iter ^ " + " ^ chunk_size_iter ^ "; " ^ iterName ^ "++) {\n" 
                )
                | Blocked -> ":p\n"
                | Strided -> ":p\n"
                | Custom -> ":q\n"

    )
*)
and stmt_emit (_ : stmt) : string = ""
(*
  match s with
    | Assign (x, e) -> "%s = %s" #% x (expr_emit e)
    | MemAssign (a, es, e) -> 
        let dims = 
          List.fold_left (fun s e -> "%s[%s]" #% s (expr_emit e)) "" es in
        "%s%s = %s" #% a dims (expr_emit e)
    | DeclAssign (typ, x, e) -> "%s %s = %s" #% (typ_emit typ) (expr_emit e)
    | If of (expr * stmt list) list
    | While of expr * stmt list
    | For of stmt * expr * stmt * (stmt list)
    | Break of string
    | Print of string
    | BsgFinish

and convert_stmt (s : stmt) : string =
    match s with
    | Decl (str1, str2) -> str1 ^ " " ^ str2 ^ ";"
    | Assign (str1, expr) -> str1 ^ (" = ") ^ (expr_emit expr) ^ ";"
    | MemAssign ((symbol, dim_1, dim_2), expr2) -> 
        symbol ^ "[" ^ (expr_emit dim_1) ^ "]" ^
        (apply_to_option dim_2 "" (fun (d : expr) : string -> "[" ^ (expr_emit d) ^ "]"))
        ^ ("= ") ^ (expr_emit expr2) ^ ";"
    | DeclAssign (str1, str2, expr) -> str1 ^ " " ^ str2 ^ (" = ") ^ (expr_emit expr) ^ ";"
        (*if(String.equal (expr_emit expr) "(x + (y * x_max))") then str1 ^ " " ^ str2 ^ (" = ") ^ "tile_id*csize;"
        else str1 ^ " " ^ str2 ^ (" = ") ^ (expr_emit expr) ^ ";"*)
    | If (i,il,s) -> (
        match s with
        | None -> ((convert_ib i) ^ (convert_iblist il))
        | Some st -> ((convert_ib i) ^ (convert_iblist il) ^ "else {\n" ^ (convert_stmtlist st) ^ "}\n")
        )
    | While (e,sl) -> "while ( " ^ (expr_emit e) ^ " ) {\n" ^ (convert_stmtlist sl) ^ "}\n"
    | For ((s1,e1,(i,e2)),sl) -> "for (" ^ (convert_stmt s1) ^ " " ^ (expr_emit e1) ^ "; " ^ i ^ "=" ^ (expr_emit e2) ^ ") {\n" ^
                            (convert_stmtlist sl) ^ "}\n"
    | For_Infer (iter, sl) -> (convert_inferred_iter iter) ^
                            (convert_stmtlist sl) ^ "}\n"
    | Break _ -> "break "
    | Print s -> "bsg_printf(" ^ s ^ ");\n"
    | BsgFinish -> "bsg_finish();\n"

and convert_ib (i : if_block) : string =
    match i with
    | (e,sl) -> "if ( " ^ (expr_emit e) ^ " ) {\n" ^ (convert_stmtlist sl) ^ "} "

and convert_stmtlist (sl : stmt list) : string =
    match sl with
    | [] -> ""
    | s::st -> ((convert_stmt s)  ^ "\n" ^ (convert_stmtlist st))

(* TODO: Add local tile memory *)
and convert_dmaps (dmaps : data_decl list) : string =
    match dmaps with
    | [] -> "//empty dmaps list\n"
    | d::dt -> 
      if List.length d.data_dims >= 2
        then (
      (typ_emit d.data_type) ^ " " ^ d.data_name ^ "[" ^ (expr_emit (List.nth d.data_dims 0)) ^ "]" ^ 
      (* add the second dimension if it exists *)
      (apply_to_option (Some (List.nth d.data_dims 1)) "" (fun (d : expr) : string -> "[" ^ (expr_emit d) ^ "]"))
      ^
      (
        match Global with
          | Global -> " __attribute__ ((section (\".dram\")));"
          | Local -> ";"
      )
     ) 
        else "" 
      ^ ("\n" ^ (convert_dmaps dt))

and convert_target (prog : program) : string =
    match prog with
    | (t, _, _, _) ->
        (*let memsize = match d with
                      | (e, _) -> (expr_emit e) in*)
        (*TODO: Hard-code chunk size for now*)
        match t with
        | [] -> ""
        | _ :: _ ->
            "int num_tiles = bsg_tiles_X * bsg_tiles_Y;\n" ^
            "int x = bsg_x;\n" ^
            "int y = bsg_y;\n"
            (*"volatile int csize = " ^ memsize ^ "/(bsg_tiles_X * bsg_tiles_Y);\n"*)

and convert_data_stmt (s : data_stmt) : string =
    match s with
    | Assign (str1, expr) -> "#define " ^ str1 ^ " " ^ (expr_emit expr)

(* stmt list that shows ups in the data section, but throw define in front of everything *)
and convert_data_stmtlist (sl : data_stmt list) =
    match sl with
    | [] -> ""
    | s::st -> ((convert_data_stmt s)  ^ "\n" ^ (convert_data_stmtlist st))

(* TODO: Remove hard-coding *)
and convert_mem (prog : program) : string =
    match prog with
    | (_, _, d, _) ->
        (convert_data_stmtlist d.constant_decls) ^ "\n" ^ (convert_dmaps d.data_decls)

and convert_codelist (cl : code list) : string =
    match cl with
    | [] -> ""
    | (t, sl)::ct ->
        match t with
        | (_, (e1, e2)) ->
         (if (e1 == X && e2 == Y) then
            ((*TODO: replace with function call, and grab code and use as function before-hand, hard-code last tile handling chunk*)
            (*"if(tile_id == num_tiles-1){\n" ^
            "csize = csize + ( dim % num_tiles );\n}\n" ^*)
            (convert_stmtlist sl)  ^ "\n" ^ convert_codelist(ct))
         else
            (convert_codelist(ct) ^ "if(tile_id == bsg_x_y_to_id(" ^ (expr_emit e1) ^ ", " ^ (expr_emit e2) ^ ")){\n" ^
            (convert_stmtlist sl)  ^ "}\n")
         )

    *)
(* can read as foreach code section in program, add an int main() and foreach code listing? *)
let convert_ast (_: program) : string = ""
  (*
    "#include \"bsg_manycore.h\"\n#include \"bsg_set_tile_x_y.h\"\n" ^
    convert_mem (prog) ^
    match prog with
    | (_, _, _, c) ->
        match c with
        | (None, cl) -> (convert_codelist cl) ^ "\n"
        | (Some sl, cl) ->
            (convert_stmtlist sl) ^
            (convert_target prog) ^
            "int main() {\n" ^ "bsg_set_tile_x_y();\n" ^ "int tile_id = bsg_x_y_to_id(bsg_x, bsg_y);\n" ^
            (convert_codelist cl) ^ "bsg_wait_while(1);\n" ^ "\n}"
*)
let generate_makefile (prog : program) : string =
    match prog.target_section with
    | [] -> ""
    | GlobalMemDecl _ :: _ -> ""
    | TileDecl tile :: _ ->
        (
          if List.length tile.tile_dims >= 2
            then
              (
                "bsg_tiles_X = " ^ (expr_emit (List.nth tile.tile_dims 0)) ^
                "\nbsg_tiles_Y = " ^ (expr_emit (List.nth tile.tile_dims 1))
              )
            else ""
        )
        ^ "\n" ^ makefile
