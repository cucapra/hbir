open Ast

let rec pretty (e : expr) : string =
    match e with
    | Literal _ -> "literal"
    | String str -> str
    | Int i -> string_of_int i
    | Id i -> i
    | X -> "x"
    | Y -> "y"
    | Mem (i, d1, d2) -> i ^ pretty d1 ^
        (apply_to_expr_option d2 "" (fun (d : expr) : string -> pretty d))
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
      (apply_to_expr_option dim_2 "" (fun (d : expr) : string -> pretty d)) 
      ^ ("= ") ^ (pretty expr2)
    | DeclAssign (_,_,_) -> "declAssign "
    | If (_,_,_) -> "if "
    | While (_,_) -> "while "
    | For (_,_) -> "for "
    | Break _ -> "break "
    | Print s -> "print(" ^ s ^ ")"
    | BsgFinish -> "bsg_finish()"

let rec eval (e : expr) : int =
    match e with
    | Literal _ -> -1
    | String _ -> -1
    | Int i -> i
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
    | (target, config, data, code) ->
        (match target with
        | (m1, t) -> (match m1 with (s1, e1, (size1, width1)) -> "target mem " ^ s1 ^ " with dim " ^ (pretty e1)
        ^ " with size " ^ size1 ^ " and width " ^ width1) ^ " " ^
                    (match t with (s2, (e2, e3), m2) -> "target tile " ^ s2 ^ " with dim " ^ (pretty e2) ^ ", " ^ (pretty e3)
                    ^ " and with memory " ^ (match m2 with
                                                | None -> "empty memory"
                                                | Some mem -> match mem with (s3, e4, (size2, width2)) ->
                                                    s3 ^ " with mem dim " ^ (pretty e4) ^ " with size " ^ size2 ^ " and width " ^ width2))
        ) ^ "\n" ^
        (match config with
        | [] -> ""
        | g::_ -> (match g with
                    | NestedGroup _ -> "nested group"
                    | GroupStmt gs -> match gs with (s1, (e1, e2), _) ->
                      "config group " ^ s1 ^ " with dim " ^ (pretty e1) ^ ", " ^ (pretty e2))
        ) ^ "\n" ^
        (match data with
        | (_, _) -> "data with dim " (*^ (pretty d)*)
        ) ^ "\n" ^
        (match code with
        | (_, codeList) -> "code with " ^ (pretty_codelist codeList)
        )


