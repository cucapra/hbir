(* types *)
type hbir_type =
    | MemoryType of int
    | TileType of int * int
    | SizeType
    | WidthType
    | GroupType of int * int
    | TemporalType of int
    | ConstType

type generic_type =
    | BoolTyp
    | IntTyp
    | FloatTyp

type sgmt =
    | Target
    | Config
    | Data
    | Code

type literal =
    | XMax
    | YMax

(* core *)
(* TODO: Should wrap in binop *)
(* TODO: Use Id elsewhere instead of string *)
type expr =
    | Literal of literal
    | String of string
    | Int of int
    | Float of float
    | X
    | Y
    | Bool of bool
    | Id of string
    (* TODO: make this a expr option list and then map down as needed *)
    | Mem of string * expr * expr option
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

(* inferred iterator from data section into the code section *)
(* iterator name (i) ~ bounds ~ layout name ~ coord x ~ coord y *)
type inferred_iterator = string * expr * string * expr * expr

(* spmd *)
type stmt =
    (*| Decl of *)
    | Decl of string * string
    | Assign of string * expr
    | MemAssign of (string * expr * expr option) * expr
    | DeclAssign of string * string * expr
    (*| If of expr * (stmt list) * (stmt option)  condition * if-block * else-block *)
    | If of if_block * (if_block list) * ((stmt list) option)
    | While of expr * (stmt list) (* condition * while-block *)
    | For of (stmt * expr * (string * expr)) * (stmt list)
    | For_Infer of inferred_iterator * stmt list
    | Break of string
    | Print of string
    | BsgFinish

and if_block = expr * (stmt list)

(* fewer statements allowed in data section TODO merge with stmt using parent_stmt? *)
type data_stmt =
    (*| Decl of *)
    | Assign of string * expr

(* Helper functions *)
let apply_to_option (b_option : 'b option) (default : 'a) (func : 'b -> 'a) : 'a =
    match b_option with
        | None -> default
        | Some extracted_b -> func extracted_b


(* hbir *)
(* move to generic decl node *)
type size_decl = string

type width_decl = string

(*type height_decl = string*)

type mem_decl = string * expr * (size_decl * width_decl)

type tile_decl = string * (expr * expr) * (mem_decl option)

type tile =  string * (expr * expr)

type temporal = int

type group_block =
    | TileWithNothing of tile
    | TileWithTemporal of tile * temporal

type group_decl =
    | NestedGroup of (string * (expr * expr) * group_decl)
    | GroupStmt of (string * (expr * expr) * group_block)

type code = tile * stmt list


(************************************************************** 

Data layout typedefs

****************************************************************)

type mem_type =
    | Global
    | Local

type mem_location =
    | Host
    | Device

(* CODE ABOVE BEING GRADUALLY RESTRUCTURED BELOW,
 * when finished, all code will be 'BELOW' *)

(* program *)
type program = target_decl list * config_decl * data_section * code_decl

(* target section *)
and target_decl = 
  GlobalMemDecl of mem_decl | 
  TileMemDecl of tile_decl

(* config section *)
and config_decl = group_decl list

(* data section *)
and data_section = {
  constant_decls : data_stmt list;
  data_decls : data_decl list
}

and data_decl = {
  data_dir : inout_dir;
  data_name : string;
  data_type : generic_type;
  data_dims : expr list;
  data_loc : location;
  data_layout : data_layout; 
}

and inout_dir = In | Out

(* this type represents the previous version of 'data_layout' in the Ast used by other files *)
and old_data_layout = string * mem_type * data_layout
and data_layout = 
    | Blocked
    | Chunked
    | Strided
    (* if custom than allow code to be written there {} *)
    | Custom

(* code section *)
and code_decl = ((stmt list) option) * code list


(* utility types *)
and location = string list
