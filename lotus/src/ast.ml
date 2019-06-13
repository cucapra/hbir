(* ------ program ------ *)
type program = {
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
  cb_code : stmt
}

and typ =
    | BoolTyp
    | IntTyp
    | FloatTyp

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
    | SeqStmt of stmt * stmt
    | VarInitStmt of typ * string * expr
    | VarAssignStmt of string * expr
    | ArrayAssignStmt of string * expr list * expr
    | IfStmt of (expr * stmt) list
    | WhileStmt of expr * stmt
    | ForStmt of string * range * stmt
    | PrintStmt of string
    | BsgFinishStmt


(* utility types *)
and location = string list

(* TODO: consider factoring out the type (typ * string * expr) *)

