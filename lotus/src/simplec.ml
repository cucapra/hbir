open Ast

(* Simple C backend to target standard GCC *)
(* Translates AST to C-like code which GCC can compile *)
(* Should we also handle invoking gcc from here to create the object file? Or let the user do this from an emitted C program *)

(*
let rec convert_expr (e : expr) : string =
    match e with
    (*TODO: Need to fix this *)
    | Literal _ -> "literal"
    | String str -> str
    | Int i -> string_of_int i
    | Float i -> string_of_float i
    | Id i -> i
    | X -> "x"
    | Y -> "y"
    | Mem (i, d1, d2) -> i ^ "[" ^ (convert_expr d1) ^ "]" ^
        (apply_to_option d2 "" (fun (d : expr) : string -> "[" ^ (convert_expr d) ^ "]"))
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
    | MemAssign ((symbol, dim_1, dim_2), expr2) ->
        symbol ^ "[" ^ (convert_expr dim_1) ^ "]" ^
        (apply_to_option dim_2 "" (fun (d : expr) : string -> "[" ^ (convert_expr d) ^ "]"))
        ^ ("= ") ^ (convert_expr expr2) ^ ";"
    | DeclAssign (str1, str2, expr) -> str1 ^ " " ^ str2 ^ (" = ") ^ (convert_expr expr) ^ ";"
    | If (i,il,s) -> (
        match s with
        | None -> ((convert_ib i) ^ (convert_iblist il))
        | Some st -> ((convert_ib i) ^ (convert_iblist il) ^ "else {\n" ^ (convert_stmtlist st) ^ "}\n")
        )
    | While (e,sl) -> "while ( " ^ (convert_expr e) ^ " ) {\n" ^ (convert_stmtlist sl) ^ "}\n"
    | For ((s1,e1,(i,e2)),sl) -> "for (" ^ (convert_stmt s1) ^ " " ^ (convert_expr e1) ^ "; " ^ i ^ "=" ^ (convert_expr e2) ^ ") {\n" ^
                            (convert_stmtlist sl) ^ "}\n"
    | For_Infer (_,_) -> "Not implemented :p"
    | Break _ -> "break "
    | Print s -> "printf(" ^ s ^ ");\n"
    | BsgFinish -> "bsg_finish();\n"

and convert_ib (i : if_block) : string =
    match i with
    | (e,sl) -> "if ( " ^ (convert_expr e) ^ " ) {\n" ^ (convert_stmtlist sl) ^ "} "

and convert_stmtlist (sl : stmt list) : string =
    match sl with
    | [] -> "//empty stmt list\n"
    | s::st -> ((convert_stmt s)  ^ "\n" ^ (convert_stmtlist st))
*)
let convert_ast (_: program) : string = ""
(*
        | (_, _, _, c) -> "int main( int argc, char * argv [] ) {\n" ^
            match c with
            | (_, cl) ->
                match cl with
                | [] -> "//empty code list\n}\n"
                | ch::_ -> match ch with
                    | (_, sl) -> (convert_stmtlist sl) ^ "}\n"
                    *)
