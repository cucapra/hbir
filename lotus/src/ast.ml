(* types *)
type hbir_type =
    | MemoryType of int
    | TileType of int * int
    | SizeType
    | WidthType
    | GroupType of int * int
    | TemporalType of int
    | ConstType

type spmd_type =
    | BoolTyp
    | IntTyp
    | FloatTyp

(* core *)
(* Should wrap in binop *)
type expr =
    | String of string
    | Int of int
    | Bool of bool
    | Id of string
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Div of expr * expr
    | Eq of expr * expr
    | Neq of expr * expr
    | Lt of expr * expr
    | Gt of expr * expr
    | Lteq of expr * expr
    | Gteq of expr * expr
    | And of expr * expr
    | Or of expr * expr

(* spmd *)
type stmt =
    (*| Decl of *)
    | Decl of string * string
    | Assign of string * expr
    | DeclAssign of string * string * expr
    (*| If of expr * (stmt list) * (stmt option)  condition * if-block * else-block *)
    | If of if_block * (if_block list) * ((stmt list) option)
    | While of expr * (stmt list) (* condition * while-block *)
    | For of (stmt * expr * (string * expr)) * (stmt list)
    | Break of string
    | Print of string

and if_block = expr * (stmt list)

(* hbir *)
(* move to generic decl node *)
type size_decl = string

type width_decl = string

type mem_decl = string * expr * (size_decl * width_decl)

type tile_decl = string * (expr * expr) * mem_decl

type tile =  string * (expr * expr)

type temporal = int

type group_block =
    | TileWithNothing of tile
    | TileWithTemporal of tile * temporal

type group_decl =
    | NestedGroup of (string * (expr * expr) * group_decl)
    | GroupStmt of (string * (expr * expr) * group_block)

type code = tile * stmt list

(* segments *)
type target_decl = mem_decl * tile_decl

type config_decl = group_decl list

(* TODO: need to add mem list *)
type data_decl = expr

type code_decl = code list

(* program *)
(* Consists of target, config, data, and code sections *)
type program = target_decl * config_decl * data_decl * code_decl