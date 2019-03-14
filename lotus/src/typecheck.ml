open Ast

exception TypeException of string

let check_program (prog : program) : unit =
    match prog with
    | (_,_,_,_) -> Printf.printf "Check"