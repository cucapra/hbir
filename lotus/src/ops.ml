open Ast

let rec pretty (e : expr) : string =
    match e with
    | Literal _ -> "literal"
    | String str -> str
    | Int i -> string_of_int i
    | Float i -> string_of_float i
    | Id i -> i
    | X -> "x"
    | Y -> "y"
    | Mem (i, d1, d2) -> i ^ pretty d1 ^
        (apply_to_option d2 "" (fun (d : expr) : string -> pretty d))
    | Plus (e1, e2) -> (pretty e1) ^ " + " ^ (pretty e2)
    | Minus (e1, e2) -> (pretty e1) ^ " - " ^ (pretty e2)
    | Times (e1, e2) -> (pretty e1) ^ " * " ^ (pretty e2)
    | Div (e1, e2) -> (pretty e1) ^ " / " ^ (pretty e2)
    | Bool b -> string_of_bool b
    | Eq (e1, e2) -> (pretty e1) ^ " == " ^ (pretty e2)
    | Neq (e1, e2) -> (pretty e1) ^ " != " ^ (pretty e2)
    | Lt (e1, e2) -> (pretty e1) ^ " < " ^ (pretty e2)
    | Gt (e1, e2) -> (pretty e1) ^ " > " ^ (pretty e2)
    | Lteq (e1, e2) -> (pretty e1) ^ " <= " ^ (pretty e2)
    | Gteq (e1, e2) -> (pretty e1) ^ " >= " ^ (pretty e2)
    | And (e1, e2) -> (pretty e1) ^ " && " ^ (pretty e2)
    | Or (e1, e2) -> (pretty e1) ^ " || " ^ (pretty e2)

let pretty_stmt (s : stmt) : string =
    match s with
    | Decl (_,_) -> "decl "
    | Assign (str1, expr) -> str1 ^ ("= ") ^ (pretty expr)
    | MemAssign ((symbol, dim_1, dim_2), expr2) -> 
      symbol ^ (pretty dim_1) ^  
      (apply_to_option dim_2 "" (fun (d : expr) : string -> pretty d)) 
      ^ ("= ") ^ (pretty expr2)
    | DeclAssign (_,_,_) -> "declAssign "
    | If (_,_,_) -> "if "
    | While (_,_) -> "while "
    | For (_,_) -> "for "
    | For_Infer (_,_) -> "for "
    | Break _ -> "break "
    | Print s -> "print(" ^ s ^ ")"
    | BsgFinish -> "bsg_finish()"

let rec eval (e : expr) : int =
    match e with
    | Literal _ -> -1
    | String _ -> -1
    | Int i -> i
    | Float _ -> -1
    | Id _ -> -1
    | X -> 0
    | Y -> 0
    | Mem (_, _, _) -> -1
    | Plus (e1, e2) -> (eval e1) + (eval e2)
    | Minus (e1, e2) -> (eval e1) - (eval e2)
    | Times (e1, e2) -> (eval e1) * (eval e2)
    | Div (e1, e2) -> (eval e1) / (eval e2)
    | Bool b -> if b then 1 else 0
    | Eq (e1, e2) -> if ((eval e1) = (eval e2)) then 1 else 0
    | Neq (e1, e2) -> if ((eval e1) <> (eval e2)) then 1 else 0
    | Lt (e1, e2) -> if ((eval e1) < (eval e2)) then 1 else 0
    | Gt (e1, e2) -> if ((eval e1) > (eval e2)) then 1 else 0
    | Lteq (e1, e2) -> if ((eval e1) <= (eval e2)) then 1 else 0
    | Gteq (e1, e2) -> if ((eval e1) >= (eval e2)) then 1 else 0
    | And (e1, e2) -> if (((eval e1) * (eval e2)) = 1) then 1 else 0
    | Or (e1, e2) -> if (((eval e1) + (eval e2)) = 1) then 1 else 0

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


