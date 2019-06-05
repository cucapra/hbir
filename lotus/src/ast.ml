(* Helper functions *)
let apply_to_option (b_option : 'b option) (default : 'a) (func : 'b -> 'a) : 'a =
    match b_option with
        | None -> default
        | Some extracted_b -> func extracted_b

(* types *)
type hbir_type =
    | MemoryType of int
    | TileType of int * int
    | SizeType
    | WidthType
    | GroupType of int * int
    | TemporalType of int
    | ConstType

and generic_type =
    | BoolTyp
    | IntTyp
    | FloatTyp

and sgmt =
    | Target
    | Config
    | Data
    | Code


(* CODE ABOVE BEING GRADUALLY RESTRUCTURED BELOW,
 * when finished, all code will be 'BELOW' *)

(* utility types *)
and location = string list

(* program *)
and program = {
  target_section : target_section; 
  config_section : config_section;
  data_section : data_section; 
  code_section : code_section;
}

(* target section *)
and target_section = target_decl list
and target_decl = 
  GlobalMemDecl of mem_decl | 
  TileDecl of tile_decl

and mem_decl = {
  mem_name: string;
  mem_dims: expr list;
  (* size and width measured in GB *)
  mem_size: int;
  mem_width: int;
}

and tile_decl = {
  tile_name: string;
  tile_dims: expr list;
  mem_decls: mem_decl list
}

(* config section *)
and config_section = top_level_group_decl list

and top_level_group_decl = {
  tile_group_name : string;
  group_decls : group_decl list
}

and group_decl = {
  group_name : string; 
  ranges : range list;
  sub_groups : group_decl list
}

and range = 
      SingletonRange of expr
    | SliceRange of expr * expr

(* data section *)
and data_section = {
  data_constant_decls : (string * expr) list;
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

and data_layout = 
    | Blocked
    | Chunked
    | Strided
    (* if custom than allow code to be written there {} *)
    | Custom

(* code section *)
and code_section = {
  code_constant_decls : stmt list;
  group_routine_decl : group_routine_decl list;
}
and group_routine_decl = {
  host_group_name : string; 
  code : stmt list
}

and expr =
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Deref of string * expr
    | BinApp of binop * expr * expr

    (* TODO: These last cases should be removed/generalized *)
    | Xmax
    | Ymax
    | X
    | Y
    | Id of string

and binop =
    | Plus
    | Minus
    | Times
    | Div
    | Eq
    | Neq
    | Lt
    | Gt
    | Lteq
    | Gteq
    | And
    | Or

(* inferred iterator from data section into the code section *)
(* iterator name (i) ~ bounds ~ layout name ~ coord x ~ coord y *)
and inferred_iterator = string * expr * string * expr * expr

(* spmd *)
and stmt =
    | Decl of string * string
    | Assign of string * expr
    | MemAssign of (string * expr * expr option) * expr
    | DeclAssign of string * string * expr
    | If of expr * stmt list
    | IfElse of expr * stmt list * stmt list
    | While of expr * stmt list
    | For of (stmt * expr * (string * expr)) * (stmt list)
    | For_Infer of inferred_iterator * stmt list
    | Break of string
    | Print of string
    | BsgFinish


(* FOR BACKWARDS COMPATIBILITY *)
(* NOTE: remove this when no longer necessary backwards compatibility*)
and old_mem_decl = string * expr * (size_decl * width_decl)
and old_tile =  string * (expr * expr)
and old_tile_decl = string * (expr * expr) * (mem_decl option)
and old_data_layout = string * mem_type * data_layout

and size_decl = string

and width_decl = string

and mem_type =
    | Global
    | Local

and mem_location =
    | Host
    | Device
