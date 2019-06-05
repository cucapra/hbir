%{
open Ast
%}

(* HBIR Keywords *)
%token TARGET
%token MEMORY
%token SIZE
%token WIDTH
%token <string> SIZEDECL
%token <string> WDECL
%token TILE
%token UNKNOWN

(* layout tokens *)
%token LAYOUT

(* memory location tokens *)
%token GLOBAL
%token LOCAL

(* memory transfer direction tokens *)
%token HOST
%token DEVICE

(* memory layout tokens *)
%token CHUNK
%token STRIDE
%token CUSTOM_DIST

%token AT
%token IN
%token OUT

%token LOCATION
%token VOLATILE

%token CONFIG
%token GROUP
%token TEMPORAL
%token BLOCK
%token X
%token Y
%token X_MAX
%token Y_MAX

%token DATA
(*%token DIM*)

%token CODE


(* SPMD Keywords *)

(* iterator keywords *)
%token WHILE
%token FOR
%token ITERATOR

%token IF
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
%token EOF

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
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
    | ~ = target; ~ = config; ~ = data; ~ = code; EOF; <>
        

(* Utility Non-Terminals*)
let dim ==
    | LEFT_BRACKET; ~ = expr; RIGHT_BRACKET;
      <>

(* Target Section *)
let target :=
    | TARGET; LEFT_BRACE;
      ts = targetDecl*;
      RIGHT_BRACE;
      <>

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
let config :=
    | CONFIG; LEFT_BRACE; ~ = top_level_decl*; RIGHT_BRACE;
      <>

let top_level_decl :=
    | IN; tile_group_name = ID; 
      LEFT_BRACE; group_decls = group_decl*; RIGHT_BRACE;
      { {tile_group_name; group_decls} }

let group_decl :=
    | GROUP; group_name = ID; AT; ~ = ranges; SEMICOLON;
      { {group_name; ranges; sub_groups = [] } }

let ranges :=
    | LEFT_PAREN; 
      ~ = separated_nonempty_list(COMMA; {}, ~ = range; <>);
      RIGHT_PAREN;
      <>

let range :=
    | x = expr; 
      { SingletonRange x }
    | x = expr; COLON; y = expr;
      { SliceRange (x, y) }


(* data section *)
let configNameLookup ==
    | CONFIG; DOT; ~ = nameLookup;
      <>

let nameLookup :=
    | ~ = separated_nonempty_list(DOT; {}, ~ = ID; <>);
      <>

let data :=
    | DATA; 
      LEFT_BRACE; 
      cs= constant_decl*;
      ds = data_decl*;
      RIGHT_BRACE;
      { { constant_decls = cs; data_decls = ds} }

let constant_decl :=
    | c = ID; EQ; e = expr; SEMICOLON;
      { Assign (c, e) }

let data_decl :=
    | ~ = data_dir; data_name = ID; COLON;
      data_type = typ; data_dims = dim*;
      LEFT_BRACE;
      LOCATION; COLON; data_loc = configNameLookup; COMMA;
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
let code :=
    | CODE; LEFT_BRACE; RIGHT_BRACE;
        { (None, []) }
    | CODE; LEFT_BRACE; cs = codeBlock*; RIGHT_BRACE;
        { (None, cs) }
    | CODE; LEFT_BRACE; s = stmt*; cs = codeBlock*; RIGHT_BRACE;
        { (Some s, cs) }

let codeBlock :=
    | t = nameLookup; LEFT_BRACE; s = stmt*; RIGHT_BRACE;
        { ((List.hd t, 
            (String "strExpr", 
            String "strExpr")), s) }

(* define how one would parse a layout stmt *)
(* parse an inferred iterator line *)
let iteratorStmt :=
    | INT; iterSym = ID; IN; ITERATOR; LT; bnd = expr; COMMA; layoutSym = ID; COMMA; xArg = expr; COMMA; yArg = expr; GT;
        { iterSym, bnd, layoutSym, xArg, yArg }

let elseIf :=
    | ELSE; IF; LEFT_PAREN; e = expr; RIGHT_PAREN; 
      LEFT_BRACE; sl = stmt*; RIGHT_BRACE;
        { (e, sl) }

let stmt :=
    | INT; id = ID; SEMICOLON;
        { Decl ("int", id) }
    | BOOL; id = ID; SEMICOLON;
        { Decl ("bool", id) }
    | FLOAT; id = ID; SEMICOLON;
        { Decl ("float", id) }
    | id = ID; EQ; e = expr; SEMICOLON;
        { Assign (id, e) }

    (* Memory statements, Do we want to allow for any number of lookups? *)

    (* 1D access *)
    | id = ID; LEFT_BRACKET; e1 = expr; RIGHT_BRACKET; EQ; e2 = expr; SEMICOLON;
        { MemAssign ((id, e1, None), e2) }
    (* 2D access *)
    | id = ID; LEFT_BRACKET; dim_1 = expr; RIGHT_BRACKET; 
               LEFT_BRACKET; dim_2 = expr; RIGHT_BRACKET; 
        EQ; e2 = expr; SEMICOLON;
        { MemAssign ((id, dim_1, Some dim_2), e2) }

    | INT; id = ID; EQ; e = expr; SEMICOLON;
        { DeclAssign ("int", id, e) }
    | BOOL; id = ID; EQ; e = expr; SEMICOLON;
        { DeclAssign ("bool", id, e) }
    | FLOAT; id = ID; EQ; e = expr; SEMICOLON;
        { DeclAssign ("float", id, e) }

    (* += ++ operators here *)
    (* TODO we have to define every mem assign for every type of op? *)
    | id = ID; PLUS; EQ; e = expr; SEMICOLON;
        { Assign (id, Plus (Id id, e))}
    (* 1D access *)
    | id = ID; LEFT_BRACKET; e1 = expr; RIGHT_BRACKET; PLUS; EQ; e2 = expr; SEMICOLON;
        { MemAssign ((id, e1, None), Plus (Mem(id, e1, None), e2)) }
    (* 2D access *)
    | id = ID; LEFT_BRACKET; dim_1 = expr; RIGHT_BRACKET; 
               LEFT_BRACKET; dim_2 = expr; RIGHT_BRACKET;  
               PLUS; EQ; e2 = expr; SEMICOLON;
        { MemAssign ((id, dim_1, Some dim_2), Plus (Mem(id, dim_1, Some dim_2), e2)) }

    | IF; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl = stmt*; RIGHT_BRACE;
        {If((e, sl), [], None) }
    | IF; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl1 = stmt*; RIGHT_BRACE;
      ELSE; LEFT_BRACE; sl2 = stmt*; RIGHT_BRACE;
        {If((e, sl1), [], Some sl2) }
    | WHILE; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl = stmt*; RIGHT_BRACE;
        {While(e, sl) }
    | FOR; LEFT_PAREN; s1 = stmt; e1 = expr; SEMICOLON; i = ID; EQ; e2=expr; RIGHT_PAREN; LEFT_BRACE; sl = stmt*; RIGHT_BRACE;
        { For((s1, e1, (i,e2)), sl) }

    (* inferred interator parsing *)
    | FOR; LEFT_PAREN; iter = iteratorStmt; RIGHT_PAREN; LEFT_BRACE; sl = stmt*; RIGHT_BRACE;
        { For_Infer( iter, sl ) }

    | PRINTF; LEFT_PAREN; s = STR; RIGHT_PAREN; SEMICOLON;
        { Print s }
    | BSG_FINISH; LEFT_PAREN; RIGHT_PAREN; SEMICOLON;
        { BsgFinish }

let expr :=
    | LEFT_PAREN; e = expr; RIGHT_PAREN;
        { e }
    | X_MAX;
        { Literal (XMax) }
    | Y_MAX;
        { Literal (YMax) }
    | X;
        { X }
    | Y;
        { Y }
    | i = INT_LITERAL;
        { Int i }
    | s = STR;
        { String s }
    | f = FLOAT_LITERAL;
        { Float f }
    | e1 = expr; PLUS; e2 = expr;
        { Plus (e1, e2) }
    | e1 = expr; MINUS; e2 = expr;
        { Minus (e1, e2) }
    | e1 = expr; TIMES; e2 = expr;
        { Times (e1, e2) }
    | e1 = expr; DIV; e2 = expr;
        { Div (e1, e2) }
    | e1 = expr; EQEQ; e2 = expr;
        { Eq (e1, e2) }
    | e1 = expr; NEQ; e2 = expr;
        { Neq (e1, e2) }
    | e1 = expr; LT; e2 = expr;
        { Lt (e1, e2) }
    | e1 = expr; LTE; e2 = expr;
        { Lteq (e1, e2) }
    | e1 = expr; GT; e2 = expr;
        { Gt (e1, e2) }
    | e1 = expr; GTE; e2 = expr;
        { Gteq (e1, e2) }
    | e1 = expr; AND; e2 = expr;
        { And (e1, e2) }
    | e1 = expr; OR; e2 = expr;
        { Or (e1, e2) }
    | TRUE;
        { Bool (true)}
    | FALSE;
        { Bool (false)}
    | DATA; DOT; i = ID;
        { Id i }
    | CONFIG; DOT; i = ID;
        { Id i }
    (* Make a new type for this to retain the index *)
    (* 1D *)
    | i = ID; LEFT_BRACKET; e = expr; RIGHT_BRACKET;
        { Mem (i, e, None) }
    (* 2D *)
    | i = ID; LEFT_BRACKET; d1 = expr; RIGHT_BRACKET; LEFT_BRACKET; d2 = expr; RIGHT_BRACKET;
        { Mem (i, d1, Some d2) }
    | i = ID;
        { Id i }


(* TODO unused: mark for removal *)
let typ :=
    | FLOAT;
        { FloatTyp }
    | BOOL;
        { BoolTyp }
    | INT;
        { IntTyp }

