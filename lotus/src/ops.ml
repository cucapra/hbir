open Ast

let rec pretty (e : expr) : string =
    match e with
      String str -> str
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Float i -> string_of_float i
    | Deref (a, i) -> a ^ pretty i
    | BinApp (binop, e1, e2) -> 
        pretty e1 ^ pretty_binop binop ^ pretty e2
    | Id i -> i
    | X -> "x"
    | Y -> "y"
    | Xmax -> "xmax"
    | Ymax -> "ymax"

and pretty_binop (binop : binop) : string =
  match binop with
      Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Lteq -> "<="
    | Gteq -> ">="
    | And -> "&&"
    | Or -> "||"

let pretty_stmt (s : stmt) : string =
    match s with
    | Decl (_,_) -> "decl "
    | Assign (str1, expr) -> str1 ^ ("= ") ^ (pretty expr)
    | MemAssign ((symbol, dim_1, dim_2), expr2) -> 
      symbol ^ (pretty dim_1) ^  
      (apply_to_option dim_2 "" (fun (d : expr) : string -> pretty d)) 
      ^ ("= ") ^ (pretty expr2)
    | DeclAssign (_,_,_) -> "declAssign "
    | If (_,_) -> "if "
    | IfElse (_,_,_) -> "if-else"
    | While (_,_) -> "while "
    | For (_,_) -> "for "
    | For_Infer (_,_) -> "for "
    | Break _ -> "break "
    | Print s -> "print(" ^ s ^ ")"
    | BsgFinish -> "bsg_finish()"

let rec eval (e : expr) : int =
    match e with
    | String _ -> -1
    | Bool b -> if b then 1 else 0
    | Int i -> i
    | Float _ -> -1
    | Id _ -> -1
    | X -> 0
    | Y -> 0
    | Xmax -> -1
    | Ymax -> -1
    | Deref (_,_) -> -1

    | BinApp (binop, e1, e2) -> 
        (eval_binop binop) (eval e1) (eval e2)

and eval_binop (binop : binop) : (int -> int -> int) =
  match binop with
      Plus  -> (+)
    | Minus -> (-)
    | Times -> (fun (x, y) -> x * y)
    | Div   ->  / 
    | Eq    -> (fun (x y) -> 
        if ((eval e1) = (eval e2)) then 1 else 0)
    | Neq   -> if ((eval e1) <> (eval e2)) then 1 else 0
    | Lt    -> if ((eval e1) < (eval e2)) then 1 else 0
    | Gt    -> if ((eval e1) > (eval e2)) then 1 else 0
    | Lteq  -> if ((eval e1) <= (eval e2)) then 1 else 0
    | Gteq  -> if ((eval e1) >= (eval e2)) then 1 else 0
    | And   -> if (((eval e1) * (eval e2)) = 1) then 1 else 0
    | Or    -> if (((eval e1) + (eval e2)) = 1) then 1 else 0

let rec pretty_stmtlist (sl : stmt list) : string =
    match sl with
    | [] -> ""
    | s::st -> ((pretty_stmt s) ^ (pretty_stmtlist st))

let rec pretty_codelist (cl : code list) : string =
    match cl with
    | [] -> ""
    | c::ct -> (match c with
        | (g, e) -> (match g with (s1, (e1, e2)) -> "group " ^ s1 ^ " with dim " ^ (pretty e1) ^ ", " ^ (pretty e2)
                                                    ^ " with code " ^ (pretty_stmtlist e))) ^ (pretty_codelist ct)

let pretty_program (p : program) : string =
    match p with
    | ([], _, _, _) -> "No target specified"
    | (GlobalMemDecl m1 :: _, _, _, _) ->
        "target mem " ^ m1.mem_name ^ " with dim " ^ (pretty (List.nth m1.mem_dims 0))
        ^ " with size " ^ (string_of_int m1.mem_size) ^ " and width " ^ (string_of_int m1.mem_width) ^ " "
    | (TileDecl t :: _, config, _, code) ->
        "target tile " ^ t.tile_name ^ " with dim " ^ (pretty (List.nth t.tile_dims 0)) ^ ", " ^ (pretty (List.nth t.tile_dims 1)) ^ " and with memory " ^ 
      let t_mem = List.nth t.mem_decls 0 in
        t.tile_name ^ " with mem dim " ^ (pretty (List.nth t.tile_dims 0)) ^ " with size " ^ (string_of_int t_mem.mem_size) ^ " and width " ^ (string_of_int t_mem.mem_width) ^ "\n" ^
        (match config with
        | [] -> ""
        | top_level_g::_ ->
            if List.length (List.hd top_level_g.group_decls).sub_groups > 0
              then "nested group"
            else "config group "
        ) ^ "\n" ^
        ("data with dim " (*^ (pretty d)*)) 
          ^ "\n" ^
        (match code with
        | (_, codeList) -> "code with " ^ (pretty_codelist codeList)
        )


