(* Output String Templates *)
let (#%) = Printf.sprintf

let indent (n : int) (indentation : string) (s : string) : string =
  let rec indentation_builder (n : int) : string = 
    if n == 0 then ""
    else indentation ^ indentation_builder (n-1) in
  let indentation : string = indentation_builder n in
  let lines : string list = String.split_on_char '\n' s in
  let indented_lines = List.map ((^) (indentation)) lines in
  String.concat "\n" indented_lines

let unindent (s : string) : string =
  String.split_on_char '\n' s
  |> List.map String.trim
  |> String.concat "\n"

type stmt_ctxt = {
  ctxt_data_decls : Ast.data_decl list
  (*ctxt_tile_decls : Ast.tile_decl list *)
}

let build_stmt_ctxt (prog : Ast.program) : stmt_ctxt = 
  {ctxt_data_decls = prog.data_section.ds_data_decls }

let rec stmt_emit (ctxt : stmt_ctxt) (stmt : Ast.stmt) : string =
  match stmt with
    | C_BlobStmt blob -> blob
    | SeqStmt (s1, s2) -> "%s\n%s" #% (stmt_emit ctxt s1) (stmt_emit ctxt s2)
    | VarInitStmt (tau, x, e) -> var_init_emit tau x e
    | VarAssignStmt (x, e) -> "%s = %s;" #% x (expr_emit e)
    | ArrayAssignStmt (a, dims, e) -> 
        "%s = %s;" #% (deref_emit a dims) (expr_emit e)
    | IfStmt guarded_stmts -> 
        begin match List.map (guarded_stmt_emit ctxt) guarded_stmts with
        | [] -> ""
        | if_gb::elif_gbs -> 
            ("if " ^ if_gb) :: (List.map ((^) "elif ") elif_gbs)
            |> String.concat "\n"
        end
    | WhileStmt (e, s) -> "while " ^ guarded_stmt_emit ctxt (e, s)
    | ForInStmt (i, (x1, x2), body) ->
      let lower_bound = 
        match x1 with
        | None -> Ast.ValExpr (Ast.IntVal 0)
        | Some x1 -> x1 in

      let upper_bound =
        match x2 with
        | None -> failwith "upper bound required in range 'for in' loop range"
        | Some x2 -> x2 in

      let for_init_stmt = Ast.VarInitStmt (Ast.IntTyp, i, lower_bound) in
      let loop_cond_expr = Ast.BinAppExpr (Ast.Lt, Ast.VarExpr i, upper_bound) in
      let inc_stmt = Ast.VarAssignStmt (i, Ast.BinAppExpr (Plus, Ast.VarExpr i, Ast.ValExpr (Ast.IntVal 1))) in

      for_emit ctxt for_init_stmt loop_cond_expr inc_stmt body
    | ForOverStmt _  -> ""
        (*
        let tensor = List.find (fun d -> String.equal d.Ast.data_name a) ctxt.ctxt_data_decls in
        let num_dims = List.length tensor.data_dims in
        let num_tiles = List.length ctxt.ctxt_tile_decls in

        let data_size_expr =
          List.fold_left
            (fun size dim -> Ast.BinAppExpr (Mul, size, dim))
            (Ast.IntExpr 1) tensor.data_dims in

        let chunk_size_expr: Ast.expr = 
          match num_dims with
          | 1 -> Ast.BinAppExpr (Div, List.hd tensor.data_dims, IntExpr num_tiles)
          | _ -> failwith (Error.unsupported_abstract_iteration num_dims) in

        let start_ix_expr = 
          let offset = 
            match start_ix with 
            | None -> Ast.IntExpr 0
            | Some start -> start in
          Ast.BinAppExpr (Plus, chunk_size_expr, BinAppExpr (Mul, offset, Ast.TileIdDerefExpr 0)) in

        let start_ix_emit = expr_emit start_ix_expr in
        let for_init_stmt = Ast.VarInitStmt (Ast.IntTyp, "i", Ast.IntExpr 0) in
        let loop_cond_expr = Ast.BinAppExpr (Ast.Lt, Ast.VarExpr "i", chunk_size_expr) in
        let inc_stmt = Ast.VarAssignStmt ("i", Ast.BinAppExpr (Plus, Ast.VarExpr "i", Ast.IntExpr 1)) in
        for_emit for_init_stmt loop_cond_expr inc_stmt body
        *)

and expr_emit (e : Ast.expr) : string =
  let binop_emit (binop : Ast.binop) : string =
    match binop with
        | Ast.Plus -> "+"
        | Ast.Minus -> "-"
        | Ast.Mul -> "*"
        | Ast.Div -> "/"
        | Ast.Mod -> "%"
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
  | ValExpr v ->
    begin match v with
    | IntVal n -> string_of_int n
    | FloatVal f -> string_of_float f
    | BoolVal b -> if b then "true" else "false"
    end
  | DerefExpr (e, es) -> deref_emit (expr_emit e) es
  | BinAppExpr (binop, e1, e2) -> 
      "(%s %s %s)" #%
         (expr_emit e1) (binop_emit binop) (expr_emit e2)
  | FunAppExpr (f, es) ->
      let args_emit = List.map expr_emit es |> String.concat ", " in
      "%s(%s)" #% f args_emit
  end

and type_emit (g : Ast.typ) : string =
    match g with
    | BoolTyp -> "boolean"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | RefTyp t -> type_emit t ^ "*"

and inout_dir_emit (dir : Ast.inout_dir) : string = 
  match dir with
  | In -> "hb_mc_memcpy_to_device"
  | Out -> "hb_mc_memcpy_to_host"

and for_emit (ctxt : stmt_ctxt) 
             (for_init : Ast.stmt) 
             (loop_cond : Ast.expr) 
             (inc_stmt : Ast.stmt) 
             (loop_body : Ast.stmt) : string =
  let for_init_emit = stmt_emit ctxt for_init in
  let loop_cond_emit = expr_emit loop_cond in
  (* Strip semicolon at the end *)
  let inc_stmt_emit = 
    stmt_emit ctxt inc_stmt 
    |> fun s -> String.sub s 0 (String.length s - 1) in
  let loop_body_emit = indent 1 " " (stmt_emit ctxt loop_body) in
  "for(%s %s; %s) {\n%s\n}" #% for_init_emit loop_cond_emit inc_stmt_emit loop_body_emit

(* a[e1]..[en] *)
and deref_emit (a : string) (dims : Ast.expr list) : string = 
  a ^ dims_emit dims

(* [e1]..[en] *)
and dims_emit (ctxt : Ast.expr list) : string = 
    List.map expr_emit ctxt |>
    List.map ((#%) "[%s]") |>
    String.concat ""

and dims_product_emit (dims : Ast.expr list) : string =
  dims
  |> List.map expr_emit
  |> String.concat " * "

and guarded_stmt_emit (ctxt : stmt_ctxt) (gs : Ast.expr * Ast.stmt) : string =
  let (e, s) = gs in
  "(%s)\n{%s\n}" #% (expr_emit e) (stmt_emit ctxt s)

and var_init_emit (typ : Ast.typ) (x : string) (e : Ast.expr) : string = 
  "%s %s = %s;" #% (type_emit typ) x (expr_emit e)

let array_access_brackets_emit (dims : string list) : string = 
  List.map ((#%) "[%s]") dims |> String.concat ""

let array_decl_emit (tau : Ast.typ) (x : string) (dims : Ast.expr list) =
  "%s %s%s;" #% 
    (type_emit tau) x (dims_emit dims)

let fun_decl_emit (ret_typ : string)
                  (f : string)
                  (pars : (string * string) list) : string =
  let pars_emit = 
    List.map (fun p -> let (x, tau) = p in "%s %s" #% tau x) pars
    |> String.concat ", " in
  "%s %s(%s);" #% ret_typ f pars_emit
  
let fun_emit (ret_typ : string) 
             (f : string) 
             (pars : (string * string) list) 
             (body : string) : string =
  let pars_emit = 
    List.map (fun p -> let (x, tau) = p in "%s %s" #% tau x) pars
    |> String.concat ", " in

  let body = indent 1 " " body in

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
