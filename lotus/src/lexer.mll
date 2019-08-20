(* Define the symbols from parser.mly *)

{
open Parser
}

(* Regular expression matchings *)
let newline = ['\n']
let whitespace = ['\t' ' ']
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
    | "width"       { WIDTH }
    | "tile"        { TILE }

    (* data distribution keywords *)
    | "chunked"     { CHUNK }
    | "strided"     { STRIDE }
    | "out"         { OUT }
    | "in"          { IN }
    | "location"    { LOCATION }
    | "config"      { CONFIG }
    | "arrange"     { ARRANGE }
    | "as"          { AS }
    | "at"          { AT }
    | "over"        { OVER }
    | "group"       { GROUP }
    | "block"       { BLOCK }
    (* TODO: Test to make sure having these here doesn't mean ids can't use these *)
    | "data"        { DATA }

    | "code"        { CODE }
    | "layout"      { LAYOUT }

    (* SPMD Keywords *)

    (* iterators *)
    | "while"       { WHILE }
    | "for"         { FOR }
    | "iterator"    { ITERATOR }
    | "in"          { IN }
    | "extern"      { EXTERN }
    | "if"          { IF }
    | "else"        { ELSE }
    | "int"         { INT }
    | "bool"        { BOOL }
    | "float"       { FLOAT }
    | "true"        { TRUE }
    | "false"       { FALSE }
    | "printf"       { PRINTF }
    | "bsg_finish" {BSG_FINISH}

    (* Symbols *)
    | "("           { LEFT_PAREN }
    | ")"           { RIGHT_PAREN }
    | "["           { LEFT_BRACKET }
    | "]"           { RIGHT_BRACKET }
    | "{"           { LEFT_BRACE }
    | "}"           { RIGHT_BRACE }
    | "#"           { POUND_SIGN }

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
    | newline            { Lexing.new_line lexbuf;
                           token lexbuf }

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
