(* Output String Templates *)
let (#%) = Printf.sprintf

let indent (n : int) (s : string) : string =
  let rec indentation_builder (n : int) : string = 
    if n == 0 then ""
    else "\t" ^ indentation_builder (n-1) in
  let indentation : string = indentation_builder n in
  let lines : string list = String.split_on_char '\n' s in
  let indented_lines = List.map ((^) (indentation)) lines in
  String.concat "\n" indented_lines

let unindent (s : string) : string =
  String.split_on_char '\n' s
  |> List.map String.trim
  |> String.concat "\n"

let rec stmt_emit (stmt : Ast.stmt) : string =
  match stmt with
    | SeqStmt (s1, s2) -> "%s;\n%s" #% (stmt_emit s1) (stmt_emit s2)
    | VarInitStmt (tau, x, e) -> var_init_emit tau x e
    | VarAssignStmt (x, e) -> "%s = %s" #% x (expr_emit e)
    | ArrayAssignStmt (a, dims, e) -> 
        "%s = %s" #% (deref_emit a dims) (expr_emit e)
    | IfStmt guarded_stmt -> 
        begin match List.map guarded_stmt_emit guarded_stmt with
        | [] -> ""
        | if_gb::elif_gbs -> 
            ("if " ^ if_gb) :: (List.map ((^) "elif ") elif_gbs)
            |> String.concat "\n"
        end
    | WhileStmt (e, s) -> "while " ^ guarded_stmt_emit (e, s)
    | ForStmt (x, range, s) -> 
        begin match range with
        | SingletonRange x1 -> 
            "\n{\n\tint %s = %s;\n\t%s\n}" #% x (expr_emit x1) (stmt_emit s)
        | SliceRange (x1, x2) -> 
            "for(int %s = %s; %s < %s; %s++)\n{\n\t%s\n}" #% 
              x (expr_emit x1) x (expr_emit x2) x (stmt_emit s)
        end
    | PrintStmt str -> "printf(\"%s\", " ^ str ^ ");"
    | BsgFinishStmt -> "bsg_finish();"

and expr_emit (e : Ast.expr) : string =
  let binop_emit (binop : Ast.binop) : string =
    match binop with
        | Ast.Plus -> "+"
        | Ast.Minus -> "-"
        | Ast.Mul -> "*"
        | Ast.Div -> "/"
        | Ast.Eq -> "=="
        | Ast.Neq -> "!="
        | Ast.Lt -> "<"
        | Ast.Gt -> ">"
        | Ast.Lteq -> "<="
        | Ast.Gteq -> ">="
        | Ast.And -> "&&"
        | Ast.Or -> "||" in

  begin match e with
  | VarExpr str -> str
  | IntExpr v -> string_of_int v
  | FloatExpr v -> string_of_float v
  | BoolExpr b -> if b then "true" else "false"
  | DerefExpr (e, es) -> deref_emit (expr_emit e) es
  | BinAppExpr (binop, e1, e2) -> 
      "%s %s %s" #%
         (expr_emit e1) (binop_emit binop) (expr_emit e2)
  end

and type_emit (g : Ast.typ) : string =
    match g with
    | BoolTyp -> "boolean"
    | IntTyp -> "int"
    | FloatTyp -> "float"

and inout_dir_emit (dir : Ast.inout_dir) : string = 
  match dir with
  | In -> "deviceToHost"
  | Out -> "hostToDevice"

and guarded_stmt_emit (gs : Ast.expr * Ast.stmt) : string =
  let (e, s) = gs in
  "(%s)\n{%s\n}" #% (expr_emit e) (stmt_emit s)

and var_init_emit (typ : Ast.typ) (x : string) (e : Ast.expr) : string = 
  "%s %s = %s;" #% (type_emit typ) x (expr_emit e)

(* a[e1]..[en] *)
and deref_emit (a : string) (dims : Ast.expr list) : string = 
  a ^ dims_emit dims

(* [e1]..[en] *)
and dims_emit (ds : Ast.expr list) : string = 
    List.map expr_emit ds |>
    List.map ((#%) "[%s]") |>
    String.concat ""

and array_access_brackets_emit (dims : string list) : string = 
  List.map ((#%) "[%s]") dims |> String.concat ""

let array_decl_emit (tau : Ast.typ) (x : string) (dims : Ast.expr list) =
  "%s %s%s;" #% 
    (type_emit tau) x (dims_emit dims)

let fun_emit (ret_typ : Ast.typ option) 
             (f : string) 
             (pars : (string * Ast.typ) list) 
             (body : string) : string =
  let ret_typ_emit =
    match ret_typ with
    | None -> "void"
    | Some tau -> type_emit tau in

  let pars_emit = 
    List.map (fun p -> let (x, tau) = p in "%s %s" #% (type_emit tau) x) pars
    |> String.concat ", " in

  let body = indent 1 body in

  "%s %s(%s)\n{\n%s\n}" #% ret_typ_emit f pars_emit body

let header_emit (header_file : string) : string = 
  "#include %s" #% header_file

let expr_macro_emit
  (var_name : string) (e : Ast.expr) : string = 
    "#define %s %s" #% var_name (expr_emit e)


let dynamic_alloc_array_emit (typ : Ast.typ)
                       (x : string)
                       (dims: Ast.expr list) : string =
  let typ_name : string = type_emit typ in
  let size_e_emitted : string = 
    List.map expr_emit dims
    |> String.concat " * " in
  "%s *%s = (%s*)malloc(%s * sizeof(%s));" #%
    typ_name x typ_name size_e_emitted typ_name


let return_emit (e : Ast.expr) : string = 
  "return %s;" #% (expr_emit e)


(* Host Emit Chunks *)
let host_main_decl_emit : string =
"int main(int argc, char *argv[]) {
\tassert(argc == 2);
\tchar *manycore_program = argv[1];

\t// Initialize and get userspace pointer to FPGA
\tuint8_t fd;
\thammaInit(&fd);"

let host_blocked_layout_init_emit : string = 
"// 2D Blocked Layout Init
int num_cores = (X2 - X1) * (Y2 - Y1);
int dim_per_core = dim / num_cores;\n"

let host_blocked_layout_send_emit (x: string)
                                 (typ: Ast.typ) 
                                 (dir : Ast.inout_dir) : string =
  let dir_str = 
    match dir with
    | In -> "Send"
    | Out -> "Receive" in
"// 2D Blocked Layout %s of Array %s
for (int y = Y1; y < Y2; y++) {
  for (int x = X1; x < X2; x++) {
    // This offset sends blocks vectors across cores
    // in column-major order
    int offset = (y-Y1) * (X2-X1) * dim_per_core +
                 (x-X1) * dim_per_core;

    hammaSymbolMemcpy(fd, x, y, manycore_program, \"%s\",
                      (void*)(%s + offset),
                      dim_per_core * sizeof(%s),
                      %s);
  }
}\n" #% dir_str x x x 
        (type_emit typ) (inout_dir_emit dir)

let host_execute_code_blocks_emit : string =
"// Run all of the tiles
hammaRunMultiple(fd, X1, Y1, X2, Y2);"




(* Device Emit Chunks *)
let device_main_decl_emit : string =
  fun_emit (Some Ast.IntTyp) "main" []
    begin unindent
      "// Sets the bsg_x and bsg_y global variables.
      bsg_set_tile_x_y();
      int num_tiles = bsg_num_tiles;
      int tile_id   = bsg_x_y_to_id( bsg_x, bsg_y );
      // each tile does the same work for now
      int start_id = 0;
      "
    end
