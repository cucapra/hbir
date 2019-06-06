open Ast

exception TypeException of string

let check_program (_: program) : unit = Printf.printf "Check"
