(* Output String Templates *)
let (#%) = Printf.sprintf

let rec expr_emit (e : Ast.expr) : string =
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
  | DerefExpr (e, es) -> deref_emit e es
  | BinAppExpr (binop, e1, e2) -> 
      "%s %s %s" #%
         (expr_emit e1) (binop_emit binop) (expr_emit e2)
  end

and deref_emit (array_expr : Ast.expr) (dims : Ast.expr list) : string = expr_emit array_expr ^ dims_emit dims

and dims_emit (ds : Ast.expr list) : string = 
    List.map expr_emit ds |>
    List.map ((#%) "[%s]") |>
    String.concat ""

let type_emit (g : Ast.typ) : string =
    match g with
    | BoolTyp -> "boolean"
    | IntTyp -> "int"
    | FloatTyp -> "float"

let inout_dir_emit (dir : Ast.inout_dir) : string = 
  match dir with
  | In -> "deviceToHost"
  | Out -> "hostToDevice"


let array_access_brackets_emit (dims : string list) : string = 
  List.map ((#%) "[%s]") dims |> String.concat ""

let array_decl_emit (tau : Ast.typ) (x : string) (dims : Ast.expr list) =
  "%s %s%s;" #% 
    (type_emit tau) x (dims_emit dims)


let header_emit (header_file : string) : string = 
  "#include %s" #% header_file

let expr_macro_emit
  (var_name : string) (e : Ast.expr) : string = 
    "#define %s %s" #% var_name (expr_emit e)


let var_init_emit (typ : Ast.typ) (x : string) (e : Ast.expr) : string = 
  "%s %s = %s;" #% (type_emit typ) x (expr_emit e)

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

