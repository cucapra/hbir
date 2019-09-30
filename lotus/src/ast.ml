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
and config_section = arrange_decl list

and arrange_decl = {
  arr_name : string;
  arr_size_vars : string * string;
  arr_groups: group_decl list
}

and group_decl = {
  gd_dim_iters : (string * expr) list;
  gd_name : string; 
  gd_row_range : string * range;
  gd_col_range : string * range;
  gd_subgroups : group_decl list
}

and range = expr option * expr option

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
  data_loc : group_pattern;
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
  cs_extern_fun_decls : fun_decl list;
  cs_code_block_decls : code_block_decl list;
}

and code_block_decl = {
  cb_group_name : group_pattern;
  cb_code : stmt
}

and typ =
    | BoolTyp
    | IntTyp
    | FloatTyp
    | RefTyp of typ

and value =
    | IntVal of int
    | FloatVal of float
    | BoolVal of bool

and expr =
    | VarExpr of string
    | ValExpr of value
    | DerefExpr of expr * (expr list)
    | BinAppExpr of binop * expr * expr
    | FunAppExpr of string * (expr list)

and binop =
    | Plus
    | Minus
    | Mul
    | Div
    | Mod
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
    | ForOverStmt of string * string * range * stmt
    | ForInStmt of string * range * stmt
    | C_BlobStmt of string


(* utility types *)
and group_pattern = (string * ix_pattern) list
and ix_elem_pattern = SymIx of string | ConcIx of int
and ix_pattern = ix_elem_pattern list

and location = string list
and parameter = string * typ
and fun_decl = (string * (parameter list) * typ)
(* TODO: consider factoring out the type (typ * string * expr) *)

