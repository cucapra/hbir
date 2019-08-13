%{
open Ast
%}

(* target section keywords *)
%token TARGET
%token MEMORY
%token SIZE
%token WIDTH
%token TILE

(* config section keywords *)
%token CONFIG
%token ARRANGE
%token AS
%token GROUP
%token AT
%token OVER

(* data section keywords *)
%token DATA
%token LOCATION
%token LAYOUT
%token BLOCK
%token CHUNK
%token STRIDE

%token IN
%token OUT

(* code section keywords *)
%token CODE
%token EXTERN
%token WHILE
%token FOR
%token ITERATOR

%token IF
%token ELIF
%token ELSE
%token INT
%token BOOL
%token FLOAT
%token TRUE
%token FALSE
%token PRINTF
%token BSG_FINISH

(* Symbols *)
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token POUND_SIGN
%token EOF

%token <string> FILENAME
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <bool> BOOL_LITERAL
%token <string> ID
%token <string> STR

%token PLUS
%token MINUS
%token TIMES
%token DIV

%token EQ
%token LT
%token LTE
%token GTE
%token GT
%token EQEQ
%token NEQ
%token AND
%token OR

%token SEMICOLON

%token DOT
%token COLON
%token COMMA

%left AND OR
%left EQEQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV

%start <Ast.program> main

%%

let main :=
    | TARGET; LEFT_BRACE;
        target_section = targetDecl*;
      RIGHT_BRACE;

      CONFIG; LEFT_BRACE; 
        config_section = arrange_decl*; 
      RIGHT_BRACE;

      DATA; LEFT_BRACE; 
        ds_constant_decls = constant_decl*;
        ds_data_decls = data_decl*;
      RIGHT_BRACE;

      CODE; LEFT_BRACE;
        cs_constant_decls = constant_decl*;
        cs_extern_fun_decls = extern_fun_decl*;
        cs_code_block_decls = code_block_decl*;
      RIGHT_BRACE;
      EOF;
      {
        {
          target_section;
          config_section;
          data_section = {
            ds_constant_decls;
            ds_data_decls
          };
          code_section = {
            cs_constant_decls;
            cs_extern_fun_decls;
            cs_code_block_decls
          } 
        }
      }
        

(* Utility Non-Terminals*)
let parens(x) := 
    | LEFT_PAREN; x = x; RIGHT_PAREN; <>

let braces(x) :=
    | LEFT_BRACE; x = x; RIGHT_BRACE; <>

let brackets(x) :=
    | LEFT_BRACKET; x = x; RIGHT_BRACKET; <>

let dim :=
    | LEFT_BRACKET; ~ = expr; RIGHT_BRACKET; <>

let nameLookup :=
    | ~ = separated_nonempty_list(DOT; {}, ~ = ID; <>); <>

let group_name_decl :=
    | group_name = ID; 
      group_size_names = parens(~ = ID; COMMA; ~ = ID; <>); <>
    | group_name = ID; { (group_name, ("", "")) }


(* Target Section *)
let targetDecl :=
    | m = mem_decl; { GlobalMemDecl m }
    | t = tile_decl; { TileDecl t }

let mem_decl :=
    | MEMORY; mem_name = ID; mem_dims = dim*; 
      LEFT_BRACE;
      SIZE; COLON; mem_size = INT_LITERAL; COMMA;
      WIDTH; COLON; mem_width = INT_LITERAL;
      RIGHT_BRACE; 
      { {mem_name; mem_dims; mem_size; mem_width} }

let tile_decl :=
    | TILE; tile_name = ID; tile_dims = dim*;
      LEFT_BRACE; mem_decls = mem_decl+; RIGHT_BRACE; 
      { {tile_name; tile_dims; mem_decls} }


(* Config section *)
let arrange_decl :=
    | ARRANGE; name = group_name_decl; AS;
      arr_groups = braces(group_decl+);
      {
        let arr_name, arr_size_vars = name in
        {arr_name; arr_size_vars; arr_groups;} 
      }

let group_decl :=
    | GROUP; group_dim_iters = group_dim_iter*;
      name = group_name_decl; AT;
      group_parent_range = parens(rows = range; COMMA; cols = range; <>);
      maybe_subgroups = braces(group_decl*)?;
      { 
        let group_name, group_size_names = name in
        let subgroups =
          match maybe_subgroups with
          | None -> []
          | Some sgs -> sgs in
        { group_dim_iters;
          group_name;
          group_size_names;
          group_parent_range;
          subgroups }
      }

let group_dim_iter :=
    | ~ = brackets(~ = ID; IN; ~ = expr; <>); <>

let range :=
    | e1 = expr?; COLON; e2 = expr?; <>
    | e = expr; { (Some e, Some e) }


(* data section *)
let data_decl :=
    | ~ = data_dir; data_name = ID; COLON;
      data_type = typ; data_dims = dim*;
      LEFT_BRACE;
      LOCATION; COLON; data_loc = nameLookup; COMMA;
      LAYOUT; COLON; ~ = data_layout;
      RIGHT_BRACE;
      { 
        {
         data_dir; 
         data_name; 
         data_type; 
         data_dims; 
         data_loc; 
         data_layout 
        } 
      } 

let data_dir :=
    | IN; 
      { In }
    | OUT;
      { Out }

let data_layout :=
    | BLOCK;
        { Blocked }
    | CHUNK;
        { Chunked }
    | STRIDE;
        { Strided }


(* code section *)
let extern_fun_decl :=
    | EXTERN; f_name = ID; 
      params = parens(separated_list(COMMA, ~ = ID; COLON; ~ = typ; <>));
      COLON; ret_typ = typ; SEMICOLON;
      <>

let constant_decl :=
  tau = typ; x = ID; EQ; e = expr; SEMICOLON; <>

let code_block_decl :=
    | cb_group_name = nameLookup;
      LEFT_BRACE; cb_code = stmt; RIGHT_BRACE;
        { {cb_group_name; cb_code} }

let binapp(binop) :=
    | e1 = expr; binop = binop; e2 = expr;
      { BinAppExpr (binop, e1, e2) }

let expr :=
    | ~ = INT_LITERAL; < IntExpr >
    | ~ = FLOAT_LITERAL; < FloatExpr >
    | ~ = BOOL_LITERAL; < BoolExpr >
    | a = ID; dims = dim+;
      { DerefExpr (VarExpr a, dims) }
    | ~ = binapp(PLUS; { Plus }); <>
    | ~ = binapp(MINUS; { Minus}); <>
    | ~ = binapp(TIMES; { Mul }); <>
    | ~ = binapp(DIV; { Div}); <>
    | ~ = binapp(EQEQ; { Eq }); <>
    | ~ = binapp(NEQ; { Neq }); <>
    | ~ = binapp(LT; { Lt }); <>
    | ~ = binapp(GT; { Gt }); <>
    | ~ = binapp(LTE; { Lteq }); <>
    | ~ = binapp(GTE; { Gteq }); <>
    | ~ = binapp(AND; { And }); <>
    | ~ = binapp(OR; { Or }); <>
    | f = ID; 
      es = parens(separated_list(COMMA, expr)); 
      < FunAppExpr >
      
    | ~ = ID; < VarExpr >
    | ~ = parens(expr); <>

let typ :=
    | FLOAT; { FloatTyp }
    | BOOL; { BoolTyp }
    | INT; { IntTyp }

    
let guarded_body := 
    | ~ = parens(expr); body = braces(stmt ); <>

let stmt :=
    | s1 = stmt; s2 = stmt; < SeqStmt >
    | ~ = stmt_without_semi; <>
    | ~ = stmt_with_semi; SEMICOLON; <>
    
let stmt_without_semi := 
    | FOR; i = ID; OVER; a = ID; ~ = brackets(range);
      body = braces(stmt);
      < ForOverStmt >
    | FOR; i = ID; IN; ~ = brackets(range); ~ = braces(stmt);
      < ForInStmt >

let stmt_with_semi := 
    | t = typ; x = ID; EQ; e = expr; 
      < VarInitStmt >
    | a = ID; coord = brackets(expr)*; EQ; e = expr;
      < ArrayAssignStmt > 
      (*
    | x = ID; EQ; e = expr;
      < VarAssignStmt >
    | a = ID; es = dim*; EQ; e = expr;
      < ArrayAssignStmt >
    | IF; if_gb = guarded_body;
      elif_gbs = preceded(ELIF, guarded_body)*;
      else_b = preceded(ELSE, braces(stmt* ))?;
      { 
        let gbs = if_gb::elif_gbs in
          match else_b with
            None -> gbs
            Some b -> IfStmt (gbs@[(Bool true, b)])
      }
    | While of expr * stmt list
    | For of stmt * expr * stmt * (stmt list)
    | Break of string
    | Print of string
    | BsgFinish

    | INT; id = ID; SEMICOLON;
        { Decl ("int", id) }
    | BOOL; id = ID; SEMICOLON;
        { Decl ("bool", id) }
    | FLOAT; id = ID; SEMICOLON;
        { Decl ("float", id) }
    | id = ID; EQ; e = expr; SEMICOLON;
        { Assign (id, e) }
*)
