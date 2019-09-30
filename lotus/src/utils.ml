type binding = string * int
type ctxt = binding list

let typecheck (_ : Ast.expr) (_ : Ast.typ) : bool = true
let typeof (_ : Ast.value) : Ast.typ = Ast.IntTyp

let int_of_value (v : Ast.value) : int =
  match v with
  | IntVal n -> n
  | _ -> failwith "value is not an int"

let expr_of_int (n : int) : Ast.expr = ValExpr (IntVal n)
let expr_of_float (f : float) : Ast.expr = ValExpr (FloatVal f)
let expr_of_bool (b : bool) : Ast.expr = ValExpr (BoolVal b)

let rec eval_expr (ctxt : ctxt) (e : Ast.expr) : Ast.value =
  match e with
  | VarExpr x ->
      begin try Ast.IntVal (List.assoc x ctxt)
      with e -> print_endline ("no binding for variable " ^ x); 
                raise e end
  | ValExpr v -> v
  | BinAppExpr (binop, e1, e2) ->
      begin

      let v1 = eval_expr ctxt e1 in
      let v2 = eval_expr ctxt e2 in

      if (typeof v1) = Ast.IntTyp && (typeof v2) = Ast.IntTyp
      then 
        let n1, n2 = match (v1, v2) with
        | IntVal n1, IntVal n2 -> n1, n2
        | _ -> failwith "only int exprs supported" in

        match binop with
        | Plus -> Ast.IntVal (n1 + n2)
        | Minus -> Ast.IntVal (n1 - n2)
        | Mul -> Ast.IntVal (n1 * n2)
        | Div -> Ast.IntVal (n1 / n2)
        | _ -> failwith "unimplemented binop"
      else failwith "only int exprs supported"
      end
  | _ -> failwith "unimplemented eval"


  let eval_int_expr (ctxt : ctxt) (e : Ast.expr) : int =
    if typecheck e Ast.IntTyp
    then eval_expr ctxt e |> int_of_value
    else failwith "range requires int type bounds"

let eval_range (ctxt : ctxt) (r : Ast.range) : int * int =
  let maybe_low_bound, maybe_up_bound = r in

  let low_bound : int = 
    match maybe_low_bound with
    | None -> 0
    | Some low_bound_expr -> eval_int_expr ctxt low_bound_expr in

  let up_bound : int = 
    match maybe_up_bound with
    | None -> low_bound + 1
    | Some up_bound_expr -> eval_int_expr ctxt up_bound_expr in

  (low_bound, up_bound) 


