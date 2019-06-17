let emit (prog: Ast.program) : string = 
  let header_emit : string = 
    ["\"f1_helper.h\""; "<stdlib.h>"]
    |> List.map Core_emit.header_emit
    |> String.concat "\n"  in

  let global_constants_emit (ts : Ast.target_section) (ds : Ast.data_section) : string = 

    let tile_size_constants_emit (td : Ast.target_decl) : string =
      match td with
      | GlobalMemDecl _ -> ""
      | TileDecl tile ->
        if List.length tile.tile_dims != 2
          then failwith Error.only_two_dims_error
          else
            [("X1", Ast.IntExpr 0);
             ("X2", List.nth tile.tile_dims 0);
             ("Y1", Ast.IntExpr 0);
             ("Y2", List.nth tile.tile_dims 1)]
            |> List.map
               (fun decl -> 
                 let (x, e) = decl in 
                 Core_emit.expr_macro_emit x e)
            |> String.concat "\n" in

    let constant_decl_emit (const_decl : Ast.typ * string * Ast.expr) =
      let (typ, x, e) = const_decl in 
      Core_emit.var_init_emit typ x e in

    ["// Tile Size Constants\n" ^
     (List.map tile_size_constants_emit ts |> String.concat "\n");
     "// Data Section Constants\n" ^
     (List.map constant_decl_emit ds.ds_constant_decls |> String.concat "\n");]
    |> String.concat "\n\n" in


  let main_def_emit (ds : Ast.data_section) : string =
    let layout_emit (d : Ast.data_decl) : string =
      match d.data_layout with
      | Blocked -> 
          [Core_emit.host_blocked_layout_init_emit d.data_name d.data_dims;
           Core_emit.host_blocked_layout_send_emit 
            d.data_name d.data_type d.data_dir] |> String.concat "\n\n"
      | _ -> failwith Error.unsupported_layout_error in

    let array_decl_emit (d : Ast.data_decl) : string =
      Core_emit.dynamic_alloc_array_emit d.data_type d.data_name d.data_dims in

    let array_decls : string = 
     "// Array Declarations\n" ^
     (List.map array_decl_emit ds.ds_data_decls |> String.concat "\n") in

    let input_array_layouts : string = 
      ds.Ast.ds_data_decls
      |> List.filter (fun d -> d.Ast.data_dir == Ast.In) 
      |> List.map layout_emit
      |> String.concat "\n\n" in

    let output_array_layouts : string = 
      ds.Ast.ds_data_decls
      |> List.filter (fun d -> d.Ast.data_dir == Ast.Out) 
      |> List.map layout_emit
      |> String.concat "\n\n" in

    let execute_blocks_emit =
      Core_emit.host_execute_code_blocks_emit in

    let return_emit = Core_emit.return_emit (Ast.IntExpr 0) in

    Core_emit.fun_emit "int" "main" [("argc", "int"); ("*argv[]", "char")]
      ([Core_emit.host_main_init_emit;
       array_decls;
       input_array_layouts;
       execute_blocks_emit;
       output_array_layouts;
       return_emit;]
      |> String.concat "\n\n") in

  header_emit ^ 
  "\n\n" ^

  global_constants_emit prog.Ast.target_section prog.Ast.data_section ^
  "\n\n" ^

  main_def_emit prog.Ast.data_section
