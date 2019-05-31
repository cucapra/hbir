(* Define the symbols from parser.mly *)

{
open Parser
}

(* Regular expression matchings *)
let whitespace = ['\t' ' ' '\n']
let comment = "//" [^ '\r' '\n']*
let digit = ['0'-'9']
let st = '1' '6'
let tt = '3' '2'
let sf = '6' '4'
let ote = '1' '2' '8'
let tfs = '2' '5' '6'
let ft = '5' '1' '2'
let sizePrefix = ['K' 'G']
let b = ['B']
let twos = ['1' '2' '4' '8']
let size = (twos | st | tt | sf | ote | tfs | ft) sizePrefix
let width = (twos | st | tt | sf | ote | tfs | ft) b
let number = '-'? digit digit*
(* https://stackoverflow.com/questions/12643009/regular-expression-for-floating-point-numbers *)
let float_number = ['0'-'9']+(['.']['0'-'9']*)?|['.']['0'-'9']+
let letter = ['a'-'z' 'A'-'Z']
let punctuation = ['.' ',' '!']
let str = '"' (whitespace|letter|punctuation)* '"'
let identifier = letter (letter | digit | '_')*

(* Token rules *)
rule token =
    parse
    (* HBIR Keywords *)
    | "target"      { TARGET }
    | "memory"      { MEMORY }
    | "size"        { SIZE }
    | size as size  { SIZEDECL (size) }
    | "width"       { WIDTH }
    | width as w    { WDECL (w) }
    | "tile"        { TILE }
    | "?"           { UNKNOWN }
    | "global"      { GLOBAL }
    | "local"       { LOCAL }
    | "host"        { HOST }
    | "device"      { DEVICE }

    (* data distribution keywords *)
    | "chunked"     { CHUNK }
    | "strided"     { STRIDE }
    | "out"         { OUT }
    | "in"          { IN }
    | "location"    { LOCATION }
    | "config"      { CONFIG }
    | "group"       { GROUP }
    | "temporal"    { TEMPORAL }
    | "block"       { BLOCK }
    (* TODO: Test to make sure having these here doesn't mean ids can't use these *)
    | "x"           { X }
    | "y"           { Y }
    | "x_max"       { X_MAX }
    | "y_max"       { Y_MAX }

    | "data"        { DATA }

    | "code"        { CODE }
    | "layout"      { LAYOUT }

    (* SPMD Keywords *)

    (* iterators *)
    | "while"       { WHILE }
    | "for"         { FOR }
    | "iterator"    { ITERATOR }
    | "in"          { IN }

    | "if"          { IF }
    | "else"        { ELSE }
    | "int"         { INT }
    | "bool"        { BOOL }
    | "float"       { FLOAT }
    | "true"        { TRUE }
    | "false"       { FALSE }
    (* TODO: Maybe implement this in a different way in the future (ie: don't handle this on the lexer/parser level) *)
    | "printf"       { PRINTF }
    (* TODO: Hacky way to implement bsg imperatives *)
    | "bsg_finish" {BSG_FINISH}

    (* Symbols *)
    | "("           { LEFT_PAREN }
    | ")"           { RIGHT_PAREN }
    | "["           { LEFT_BRACKET }
    | "]"           { RIGHT_BRACKET }
    | "{"           { LEFT_BRACE }
    | "}"           { RIGHT_BRACE }

    | "+"           { PLUS }
    | "-"           { MINUS }
    | "*"           { TIMES }
    | "/"           { DIV }

    | "="           { EQ }
    | "<"           { LT }
    | "<="          { LTE }
    | ">="          { GTE }
    | ">"           { GT }
    | "=="          { EQEQ }
    | "!="          { NEQ }
    | "&&"          { AND }
    | "||"          { OR }

    | ";"           { SEMICOLON }

    | "."           { DOT }
    | ":"           { COLON }
    | ","           { COMMA }

    (* Whitespace *)
    | whitespace+        { token lexbuf }

    | comment            { token lexbuf }

    (* Integers *)
    | number as number   { INT_LITERAL (int_of_string number) }

    (* Float (matched by regex defined at the top) *)
    | float_number as number   { FLOAT_LITERAL (float_of_string number) }

    (* String *)
    | str as str { STR (str) }

    (* Identifiers *)
    | identifier as id { ID (id) }

    | eof        { EOF }
