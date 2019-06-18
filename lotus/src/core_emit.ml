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
    | SeqStmt (s1, s2) -> "%s\n%s" #% (stmt_emit s1) (stmt_emit s2)
    | VarInitStmt (tau, x, e) -> var_init_emit tau x e
    | VarAssignStmt (x, e) -> "%s = %s;" #% x (expr_emit e)
    | ArrayAssignStmt (a, dims, e) -> 
        "%s = %s;" #% (deref_emit a dims) (expr_emit e)
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
        | SliceRange (x1, x2) -> for_emit x x1 x2 (stmt_emit s)
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
  | In -> "hb_mc_memcpy_to_device"
  | Out -> "hb_mc_memcpy_to_host"

and for_emit (loop_var : string) (lower_bound : Ast.expr) (upper_bound : Ast.expr) (loop_body : string) =
  "for(int %s = %s; %s < %s; %s++) {\n%s\n}" #% 
    loop_var (expr_emit lower_bound) loop_var (expr_emit upper_bound) loop_var (indent 1 loop_body)

(* a[e1]..[en] *)
and deref_emit (a : string) (dims : Ast.expr list) : string = 
  a ^ dims_emit dims

(* [e1]..[en] *)
and dims_emit (ds : Ast.expr list) : string = 
    List.map expr_emit ds |>
    List.map ((#%) "[%s]") |>
    String.concat ""

and dims_product_emit (dims : Ast.expr list) : string =
  dims
  |> List.map expr_emit
  |> String.concat " * "

and guarded_stmt_emit (gs : Ast.expr * Ast.stmt) : string =
  let (e, s) = gs in
  "(%s)\n{%s\n}" #% (expr_emit e) (stmt_emit s)

and var_init_emit (typ : Ast.typ) (x : string) (e : Ast.expr) : string = 
  "%s %s = %s;" #% (type_emit typ) x (expr_emit e)

let array_access_brackets_emit (dims : string list) : string = 
  List.map ((#%) "[%s]") dims |> String.concat ""

let array_decl_emit (tau : Ast.typ) (x : string) (dims : Ast.expr list) =
  "%s %s%s;" #% 
    (type_emit tau) x (dims_emit dims)

let fun_emit (ret_typ : string) 
             (f : string) 
             (pars : (string * string) list) 
             (body : string) : string =
  let pars_emit = 
    List.map (fun p -> let (x, tau) = p in "%s %s" #% tau x) pars
    |> String.concat ", " in

  let body = indent 1 body in

  "%s %s(%s)\n{\n%s\n}" #% ret_typ f pars_emit body

let header_emit (header_file : string) : string = 
  "#include %s" #% header_file

let expr_macro_emit
  (var_name : string) (s : string) : string = 
    "#define %s %s" #% var_name s


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

let host_fun_init_emit (fun_name : string) : string =
  "int err;

  // Initialize the device.
  hb_mc_device_t device;
  hb_mc_dimension_t mesh_dim = {.x = MESH_X, .y = MESH_Y};
  err = hb_mc_device_init(&device, \"~/\", 0,  mesh_dim);
  if (err) return err;

  // Load the `%s.riscv` program.
  err = hb_mc_device_program_init(&device, \"%s.riscv\", \"~/\", 0);
  if (err) return err;" #% fun_name fun_name
  |> unindent

(*
let host_blocked_layout_init_emit (a : string) (dims : Ast.expr list) : string =
  ["// 2D Blocked Layout Init of Input: %s" #% a;
   "int dim_%s = %s;" #% a (dims |> List.map expr_emit |> String.concat " * ");
   "int dim_%s_per_core = dim_%s / (X2 - X1) * (Y2 - Y1);" #% a a]
   |> String.concat "\n"

let host_blocked_layout_send_emit (x: string)
                                 (typ: Ast.typ) 
                                 (dir : Ast.inout_dir) : string =
  let dir_str =
    match dir with
    | In -> "Send"
    | Out -> "Receive" in

  ["// 2D Blocked Layout %s of Array %s\n" #% dir_str x ^
   for_emit "y" (VarExpr "Y1") (VarExpr "Y2") 
     (for_emit "x" (VarExpr "X1") (VarExpr "X2")
     (["// This offset sends blocks vectors across cores";
       "// in column-major order";
       "int offset = (y-Y1) * (X2-X1) * dim_%s_per_core +" #% x;
       "(x-X1) * dim_%s_per_core;" #% x;
       "hammaSymbolMemcpy(fd, x, y, manycore_program, \"%s\",
                          (void* )(%s + offset),
                          dim_%s_per_core * sizeof(%s),
                          %s);" #% x x x (type_emit typ) (inout_dir_emit dir);] |> String.concat "\n"));
     ]
  |> String.concat "\n"
   

let host_execute_code_blocks_emit : string =
  ["// Run all of the tiles";
   "hammaRunMultiple(fd, X1, Y1, X2, Y2);"]
  |> String.concat "\n"

*)
(* Device Emit Chunks *)
let device_main_decl_emit : string =
  fun_emit "void" "main" []
    begin unindent
      "// Sets the bsg_x and bsg_y global variables.
      bsg_set_tile_x_y();
      int num_tiles = bsg_num_tiles;
      int tile_id   = bsg_x_y_to_id( bsg_x, bsg_y );
      // each tile does the same work for now
      int start_id = 0;
      "
    end
