(* types *)
type hbir_type =
    | MemoryType of int
    | TileType of int * int
    | SizeType
    | WidthType
    | GroupType of int * int
    | TemporalType of int
    | ConstType

and typ =
    | BoolTyp
    | IntTyp
    | FloatTyp


(* CODE ABOVE BEING GRADUALLY RESTRUCTURED BELOW,
 * when finished, all code will be 'BELOW' *)

(* utility types *)
and location = string list

(* ------ program ------ *)
and program = {
  target_section : target_section; 
  config_section : config_section;
  data_section : data_section; 
  code_section : code_section;
}

(* ------ target section ------ *)
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

(* ------ config section ------ *)
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

(* ------ data section ------ *)
and data_section = {
  ds_constant_decls : (typ * string * expr) list;
  ds_data_decls : data_decl list
}

and data_decl = {
  data_dir : inout_dir;
  data_name : string;
  data_type : typ;
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

(* ------ code section ------ *)
and code_section = {
  cs_constant_decls : (typ * string * expr) list;
  cs_code_block_decls : code_block_decl list;
}

and code_block_decl = {
  cb_group_name : location;
  cb_code : stmt list
}

and expr =
    | VarExpr of string
    | IntExpr of int
    | FloatExpr of float
    | BoolExpr of bool
    | DerefExpr of expr * (expr list)
    | BinAppExpr of binop * expr * expr

and binop =
    | Plus
    | Minus
    | Mul
    | Div
    | Eq
    | Neq
    | Lt
    | Gt
    | Lteq
    | Gteq
    | And
    | Or

and stmt =
    | VarInitStmt of typ * string * expr
    | VarAssignStmt of string * expr
    | ArrayAssignStmt of string * expr list * expr
    | IfStmt of (expr * stmt list) list
    | WhileStmt of expr * stmt list
    | ForStmt of string * range * (stmt list)
    | PrintStmt of string
    | BsgFinishStmt


(* FOR BACKWARDS COMPATIBILITY *)
(* NOTE: remove this when no longer necessary backwards compatibility*)
(* 
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
    *)
