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
%token IN
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

%left AND OR
%left EQEQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV

%start <Ast.program> main

%%

(* Rules *)

(* TODO: Should probably rename things to decl for high level *)
main:
    | targ = target; conf = config; dat = data; cod = code; EOF
        { (targ, conf, dat, cod) }

target:
    | TARGET; LEFT_BRACE; MEMORY; mem = memory; TILE; ti = tile; RIGHT_BRACE
        { (mem, ti) }

(* Possible ways to parse a memory line in the target section *)
memory:
    | id = ID; LEFT_BRACKET; num = expr; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; size = SIZEDECL; SEMICOLON; WIDTH; width = WDECL; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, num, (size, width)) }
    | id = ID; LEFT_BRACKET; UNKNOWN; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; size = SIZEDECL; SEMICOLON; WIDTH; width = WDECL; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, String "?", (size, width)) }
    | id = ID; LEFT_BRACKET; num = expr; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; UNKNOWN; SEMICOLON; WIDTH; width = WDECL; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, num, ("?", width)) }
    | id = ID; LEFT_BRACKET; num = expr; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; size = SIZEDECL; SEMICOLON; WIDTH; UNKNOWN; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, num, (size, "?")) }
    | id = ID; LEFT_BRACKET; UNKNOWN; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; UNKNOWN; SEMICOLON; WIDTH; width = WDECL; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, String "?", ("?", width)) }
    | id = ID; LEFT_BRACKET; UNKNOWN; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; size = SIZEDECL; SEMICOLON; WIDTH; UNKNOWN; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, String "?", (size, "?")) }
    | id = ID; LEFT_BRACKET; num = expr; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; UNKNOWN; SEMICOLON; WIDTH; UNKNOWN; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, num, ("?", "?")) }
    | id = ID; LEFT_BRACKET; UNKNOWN; RIGHT_BRACKET;
      LEFT_BRACE; SIZE; UNKNOWN; SEMICOLON; WIDTH; UNKNOWN; SEMICOLON; RIGHT_BRACE; SEMICOLON
        { (id, String "?", ("?", "?")) }

tile:
    | id = ID; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; LEFT_BRACKET; num2 = expr; RIGHT_BRACKET; LEFT_BRACE; MEMORY; m = memory; RIGHT_BRACE
        { (id, (num1, num2), Some m)}
    | id = ID; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; LEFT_BRACKET; num2 = expr; RIGHT_BRACKET; SEMICOLON
        { (id, (num1, num2), None)}

tileDecl:
    (* TODO: Remove this where I temporarily return -1 *)
    | TILE; TARGET; DOT; id = ID; LEFT_BRACKET; X; RIGHT_BRACKET; LEFT_BRACKET; Y; RIGHT_BRACKET; SEMICOLON
        { (id, (X, Y)) }
    | CONFIG; DOT; id = ID; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; LEFT_BRACKET; num2 = expr; RIGHT_BRACKET
        { (id, (num1, num2))}

config:
    | CONFIG; LEFT_BRACE; gr = groupList; RIGHT_BRACE
        { (gr) }

groupBlk:
    | t = tileDecl
        { TileWithNothing t }
    | t = tileDecl;
      TEMPORAL; GROUP; BLOCK; LEFT_BRACKET; num3 = INT_LITERAL; RIGHT_BRACKET; SEMICOLON
        { TileWithTemporal (t, num3) }

groupList:
    | g = group
        { g::[] }
    | g1 = groupList; g2 = group
        { g1@(g2::[]) }

group:
    | GROUP; id = ID; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; LEFT_BRACKET; num2 = expr; RIGHT_BRACKET; LEFT_BRACE; g1 = groupBlk; RIGHT_BRACE; SEMICOLON
        { GroupStmt (id, (num1, num2), g1)}
    | GROUP; id = ID; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; LEFT_BRACKET; num2 = expr; RIGHT_BRACKET; LEFT_BRACE; g2 = group; RIGHT_BRACE; SEMICOLON
        { NestedGroup (id, (num1, num2), g2)}

(* code in the data section -> parse a mix of dimension decl and data map decls *)
(* prob only want to allow subset of the language! *)
data:
    (* TODO: Allow in any order, need to make into list parse where list can contain
    any of the fields below!  *)
    | DATA; LEFT_BRACE; sl = dataStmtList; dl = dataMaps; RIGHT_BRACE
        { (sl, dl, None) }
    | DATA; LEFT_BRACE; sl = dataStmtList; ly = layoutStmt; dl = dataMaps; RIGHT_BRACE
        { (sl, dl, Some ly) }

memType:
    | GLOBAL; SEMICOLON
        { Global }
    | LOCAL; SEMICOLON
        { Local }

memLocation:
    | HOST; SEMICOLON
        { Host }
    | DEVICE; SEMICOLON
        { Device }

distributePolicy:
    | CHUNK; SEMICOLON
        { Chunked }
    | STRIDE; SEMICOLON
        { Strided }

(* TODO: Currently hard-code chunked memory type *)
(* TODO: Seems like we should be able to reuse some parsing chunks rather than repeat *)
(* TODO: What's the point of saying block[target.g.x_max] *)
dataMap:
    | id = ID; COLON; t = typ; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; EQ;
               BLOCK; LEFT_BRACKET; s1 = sgmts; DOT; id1 = ID; DOT; num2 = expr; RIGHT_BRACKET;
               LEFT_BRACE; s2 = sgmts; DOT; id2 = ID; LEFT_BRACKET; X; RIGHT_BRACKET; SEMICOLON; mt = memType; CHUNK; SEMICOLON; ml = memLocation; RIGHT_BRACE; SEMICOLON
       {mt, ml, id, t, (num1, None), (id1, None), (num2, None), (s1, Some s2, None), id2}
    
    | id = ID; COLON; t = typ; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; LEFT_BRACKET; num2 = expr; RIGHT_BRACKET; EQ;
               BLOCK; LEFT_BRACKET; s1 = sgmts; DOT; id1 = ID; DOT; num3 = expr; RIGHT_BRACKET;
               LEFT_BRACE; s2 = sgmts; DOT; id2 = ID; LEFT_BRACKET; X; RIGHT_BRACKET; SEMICOLON; mt = memType; CHUNK; SEMICOLON; ml = memLocation; RIGHT_BRACE; SEMICOLON
       {mt, ml, id, t, (num1, Some num2), (id1, None), (num3, None), (s1, Some s2, None), id2}

    | id = ID; COLON; t = typ; LEFT_BRACKET; num1 = expr; RIGHT_BRACKET; LEFT_BRACKET; num2 = expr; RIGHT_BRACKET; EQ;
               BLOCK; LEFT_BRACKET; s1 = sgmts; DOT; id1 = ID; DOT; num3 = expr; RIGHT_BRACKET;
                      LEFT_BRACKET; s2 = sgmts; DOT; id2 = ID; DOT; num4 = expr; RIGHT_BRACKET;
               LEFT_BRACE; s3 = sgmts; DOT; id3 = ID; LEFT_BRACKET; X; RIGHT_BRACKET; SEMICOLON; mt = memType; CHUNK; SEMICOLON; ml = memLocation; RIGHT_BRACE; SEMICOLON
       {mt, ml, id, t, (num1, Some num2), (id1, Some id2), (num3, Some num4), (s1, Some s3, Some s2), id3}


    (* 2d handler 
        Syntax <type>[<x>][<y>] = block[<targ>][<targ>]
            <targ>
            <global/local>
            <chunked>
            <host/device>
    *)

    | id = ID; COLON; t = typ; 
                    LEFT_BRACKET; dim_x = expr; RIGHT_BRACKET; 
                    LEFT_BRACKET; dim_y = expr; RIGHT_BRACKET;
                    EQ;
                BLOCK; LEFT_BRACKET; seg_x = sgmts; DOT; id_x = ID; DOT; num1 = expr; RIGHT_BRACKET;
                LEFT_BRACKET; seg_y = sgmts; DOT; id_y = ID; DOT; num2 = expr; RIGHT_BRACKET;

               LEFT_BRACE; s2 = sgmts; DOT; id2 = ID; LEFT_BRACKET; X; RIGHT_BRACKET; SEMICOLON; mt = memType; CHUNK; SEMICOLON; ml = memLocation; RIGHT_BRACE; SEMICOLON
       {mt, ml, id, t, (dim_x, Some dim_y), (id_x, Some id_y), (num1, Some num2), (seg_x, Some seg_y, Some), id2}

dataMaps:
    | d = dataMap
        { d::[] }
    | d1 = dataMaps; d2 = dataMap
        { d1@(d2::[]) }

(* TODO: Once you enter code section, the parser should enter a state of just checking for SPMD (think in how yacc worked in Java) *)
code:
    | CODE; LEFT_BRACE; RIGHT_BRACE
        { (None, []) }
    | CODE; LEFT_BRACE; c = codeList; RIGHT_BRACE
        { (None, c) }
    | CODE; LEFT_BRACE; s = stmtList; c = codeList; RIGHT_BRACE
        { (Some s, c) }

codeList:
    | c = codeBlock
        { c::[] }
    | c1 = codeList; c2 = codeBlock
        { c1@(c2::[]) }

codeBlock:
    | t = tileDecl; LEFT_BRACE; s = stmtList; RIGHT_BRACE
        { (t, s)}

stmtList:
    | s = stmt
        { s::[] }
    | s1 = stmtList; s2 = stmt
        {s1@(s2::[]) }

dataStmtList:
    | s = dataStmt
        { s::[] }
    | s1 = dataStmtList; s2 = dataStmt
        {s1@(s2::[]) }

(* define how one would parse a layout stmt *)
layoutStmt :
    | LAYOUT; layout_name = ID; LEFT_BRACE; mt = memType; dp = distributePolicy; RIGHT_BRACE; SEMICOLON
        { (layout_name, mt, dp) }

(* parse an inferred iterator line *)
iteratorStmt :
    | INT; iterSym = ID; IN; ITERATOR; LT; bnd = expr; COMMA; layoutSym = ID; COMMA; xArg = expr; COMMA; yArg = expr; GT
        { iterSym, bnd, layoutSym, xArg, yArg }

elseIfList:
    | e = elseIf
        { e::[] }
    | e1 = elseIfList; e2 = elseIf
        { e1@(e2::[]) }

elseIf:
    | ELSE; IF; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl = stmtList; RIGHT_BRACE
        { (e, sl) }

(* only assigns allowed for now *)
dataStmt:
    | id = ID; EQ; e = expr; SEMICOLON
        { Assign (id, e) }

stmt:
    | INT; id = ID; SEMICOLON
        { Decl ("int", id) }
    | BOOL; id = ID; SEMICOLON
        { Decl ("bool", id) }
    | FLOAT; id = ID; SEMICOLON
        { Decl ("float", id) }
    | id = ID; EQ; e = expr; SEMICOLON
        { Assign (id, e) }

    (* Memory statements, Do we want to allow for any number of lookups? *)

    (* 1D access *)
    | id = ID; LEFT_BRACKET; e1 = expr; RIGHT_BRACKET; EQ; e2 = expr; SEMICOLON
        { MemAssign ((id, e1, None), e2) }
    (* 2D access *)
    | id = ID; LEFT_BRACKET; dim_1 = expr; RIGHT_BRACKET; 
               LEFT_BRACKET; dim_2 = expr; RIGHT_BRACKET; 
        EQ; e2 = expr; SEMICOLON
        { MemAssign ((id, dim_1, Some dim_2), e2) }

    | INT; id = ID; EQ; e = expr; SEMICOLON
        { DeclAssign ("int", id, e) }
    | BOOL; id = ID; EQ; e = expr; SEMICOLON
        { DeclAssign ("bool", id, e) }
    | FLOAT; id = ID; EQ; e = expr; SEMICOLON
        { DeclAssign ("float", id, e) }

    (* += ++ operators here *)
    (* TODO we have to define every mem assign for every type of op? *)
    | id = ID; PLUS; EQ; e = expr; SEMICOLON
        { Assign (id, Plus (Id id, e))}
    (* 1D access *)
    | id = ID; LEFT_BRACKET; e1 = expr; RIGHT_BRACKET; PLUS; EQ; e2 = expr; SEMICOLON
        { MemAssign ((id, e1, None), Plus (Mem(id, e1, None), e2)) }
    (* 2D access *)
    | id = ID; LEFT_BRACKET; dim_1 = expr; RIGHT_BRACKET; 
               LEFT_BRACKET; dim_2 = expr; RIGHT_BRACKET;  
               PLUS; EQ; e2 = expr; SEMICOLON
        { MemAssign ((id, dim_1, Some dim_2), Plus (Mem(id, dim_1, Some dim_2), e2)) }

    | IF; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl = stmtList; RIGHT_BRACE
        {If((e, sl), [], None) }
    | IF; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl1 = stmtList; RIGHT_BRACE;
      ELSE; LEFT_BRACE; sl2 = stmtList; RIGHT_BRACE
        {If((e, sl1), [], Some sl2) }
    | IF; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl1 = stmtList; RIGHT_BRACE;
      el = elseIfList
        {If((e, sl1), el, None) }
    | IF; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl1 = stmtList; RIGHT_BRACE;
      el = elseIfList
      ELSE; LEFT_BRACE; sl2 = stmtList; RIGHT_BRACE
        {If((e, sl1), el, Some sl2) }
    | WHILE; LEFT_PAREN; e = expr; RIGHT_PAREN; LEFT_BRACE; sl = stmtList; RIGHT_BRACE
        {While(e, sl) }
    | FOR; LEFT_PAREN; s1 = stmt; e1 = expr; SEMICOLON; i = ID; EQ; e2=expr; RIGHT_PAREN; LEFT_BRACE; sl = stmtList; RIGHT_BRACE
        { For((s1, e1, (i,e2)), sl) }

    (* inferred interator parsing *)
    | FOR; LEFT_PAREN; iter = iteratorStmt; RIGHT_PAREN; LEFT_BRACE; sl = stmtList; RIGHT_BRACE
        { For_Infer( iter, sl ) }

    | PRINTF; LEFT_PAREN; s = STR; RIGHT_PAREN; SEMICOLON
        { Print s }
    | BSG_FINISH; LEFT_PAREN; RIGHT_PAREN; SEMICOLON
        { BsgFinish }

expr:
    | LEFT_PAREN; e = expr; RIGHT_PAREN
        { e }
    | X_MAX
        { Literal (XMax) }
    | Y_MAX
        { Literal (YMax) }
    | X
        { X }
    | Y
        { Y }
    | i = INT_LITERAL
        { Int i }
    | s = STR
        { String s }
    | f = FLOAT_LITERAL
        { Float f }
    | e1 = expr; PLUS; e2 = expr
        { Plus (e1, e2) }
    | e1 = expr; MINUS; e2 = expr
        { Minus (e1, e2) }
    | e1 = expr; TIMES; e2 = expr
        { Times (e1, e2) }
    | e1 = expr; DIV; e2 = expr
        { Div (e1, e2) }
    | e1 = expr; EQEQ; e2 = expr
        { Eq (e1, e2) }
    | e1 = expr; NEQ; e2 = expr
        { Neq (e1, e2) }
    | e1 = expr; LT; e2 = expr
        { Lt (e1, e2) }
    | e1 = expr; LTE; e2 = expr
        { Lteq (e1, e2) }
    | e1 = expr; GT; e2 = expr
        { Gt (e1, e2) }
    | e1 = expr; GTE; e2 = expr
        { Gteq (e1, e2) }
    | e1 = expr; AND; e2 = expr
        { And (e1, e2) }
    | e1 = expr; OR; e2 = expr
        { Or (e1, e2) }
    | TRUE
        { Bool (true)}
    | FALSE
        { Bool (false)}
    | DATA; DOT; i = ID
        { Id i }
    | CONFIG; DOT; i = ID
        { Id i }
    (* Make a new type for this to retain the index *)
    (* 1D *)
    | i = ID; LEFT_BRACKET; e = expr; RIGHT_BRACKET;
        { Mem (i, e, None) }
    (* 2D *)
    | i = ID; LEFT_BRACKET; d1 = expr; RIGHT_BRACKET; LEFT_BRACKET; d2 = expr; RIGHT_BRACKET;
        { Mem (i, d1, Some d2) }
    | i = ID
        { Id i }


(* TODO unused: mark for removal *)
typ:
    | FLOAT
        { FloatTyp }
    | BOOL
        { BoolTyp }
    | INT
        { IntTyp }

sgmts:
    | TARGET
        { Target }
    | CONFIG
        { Config }
    | DATA
        { Data }
    | CODE
        { Code }
