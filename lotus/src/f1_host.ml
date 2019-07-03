let emit (prog: Ast.program) (filename : string) : string =
  let header_emit : string =
    ["<bsg_manycore_cuda.h>"; "<stdio.h>"; "<stdlib.h>"]
    |> List.map Core_emit.header_emit
    |> String.concat "\n"  in

  let global_constants_emit (ts : Ast.target_section) (ds : Ast.data_section) : string =

    let framework_constants_emit : string =
      [("_BSD_SOURCE", "");
       ("_XOPEN_SOURCE", "500");]
      |> List.map
         (fun decl -> 
           let (x, e) = decl in 
           Core_emit.expr_macro_emit x e)
      |> String.concat "\n" in

    let tile_size_constants_emit (td : Ast.target_decl) : string =
      match td with
      | GlobalMemDecl _ -> ""
      | TileDecl tile ->
        if List.length tile.tile_dims != 2
          then failwith Error.only_two_dims_error
          else
            [("MESH_X", List.nth tile.tile_dims 0 |> Core_emit.expr_emit);
             ("MESH_Y", List.nth tile.tile_dims 1 |> Core_emit.expr_emit);]
            |> List.map
               (fun decl -> 
                 let (x, e) = decl in 
                 Core_emit.expr_macro_emit x e)
            |> String.concat "\n" in

    let constant_decl_emit (const_decl : Ast.typ * string * Ast.expr) =
      let (_, x, e) = const_decl in 
      Core_emit.expr_macro_emit x (Core_emit.expr_emit e) in

    ["// Framework constants (aka, incantations)\n" ^
      framework_constants_emit;
      "// Tile Size Constants\n" ^
     (List.map tile_size_constants_emit ts |> String.concat "\n");
     "// Data Section Constants\n" ^
     (List.map constant_decl_emit ds.ds_constant_decls |> String.concat "\n");]
    |> String.concat "\n\n" in


    let (#%) = Core_emit.(#%) in

    let error_cascade (ds : string list) : string =
      match ds with 
      | [] -> ""
      | x::xs -> 
          ("err  = %s;" #% x) ::
          (List.map (fun s -> "err |= %s;" #% s) xs)
          |> String.concat "\n" in

    let error_check_emit (err : string) : string =
      let if_emit cond body = 
        "if(%s){\n%s\n}" #% cond (Core_emit.indent 1 body) in
      if_emit "err"
        ("fprintf(stderr, \"%s\"); 
          return err;" #% err
         |> Core_emit.unindent) in

  let do_fun_def_emit (fun_name : string) (ds : Ast.data_decl list) : string =
    let eva_init_emit : string =
      let eva_init_comment : string = Core_emit.unindent
        "// Allocate space on the device for the three arguments we'll pass to the
         // function. \"EVA\" is for \"endpoint virtual address,\" and it represents
         // an address in the device's memory." in

      let eva_decls : string =
        "eva_t %s;" #% (ds
          |> List.map (fun d -> (d.Ast.data_name ^ "_addr"))
          |> String.concat ", ") in

      let eva_dyn_init : string =
        (*TODO: generalize hardcoded int32_t *)
        ds
        |> List.map (fun d ->
             "hb_mc_device_malloc(&device, %s * sizeof(int32_t), &%s_addr)" #% 
              (Core_emit.dims_product_emit d.Ast.data_dims) d.Ast.data_name)  
        |> error_cascade in

      [eva_init_comment;
       eva_decls; 
       eva_dyn_init;
       error_check_emit "hb_mc_device_malloc failed"]
      |> String.concat "\n" in

    let hb_mc_device_memcpy_emit (d : Ast.data_decl) : string =
      let name = d.Ast.data_name in
      let size_expr_emit = Core_emit.dims_product_emit d.Ast.data_dims in
      let inout_dir_emit = Core_emit.inout_dir_emit d.Ast.data_dir in

      let eva_template s = "(void*)((intptr_t)%s_addr)" #% s in
      let memcpy_template dst src = 
        "hb_mc_device_memcpy(&device, %s,
          \t\t%s, %s * sizeof(int32_t), %s)" 
          #% dst src size_expr_emit inout_dir_emit in

      match d.Ast.data_dir with
      | In -> memcpy_template (eva_template name) name
      | Out -> memcpy_template name (eva_template name) in

    let input_send_emit : string =
      let input_send_comment : string = Core_emit.unindent
        "// Copy input data into the newly allocated space.
        // TK Why does the `eva_t` need to be converted to a `void*` here?
        // Shouldn't this function take an `eva_t` as an argument?" in

      let input_send_memcpy_emit : string =
        ds
        |> List.filter (fun d -> d.Ast.data_dir == Ast.In)
        |> List.map hb_mc_device_memcpy_emit
        |> error_cascade in

      let grid_init_emit : string =
        let grid_init_args_comment : string = Core_emit.unindent
          "// The arguments to pass to the device-side function. While the type here
          // is uint32_t, these are all actually pointers---arguments must be
          // word-sized numbers." in

        let grid_init_comment : string = Core_emit.unindent
          "// Set up the tile group, dimensions, and function to call. The last two
          // arguments to `hb_mc_grid_init` specify the arguments to the `add`
          // function in the device code." in

        let grid_init_args_emit : string =
          "uint32_t args[] = {%s};" #%
            (ds
             |> List.map (fun d -> d.Ast.data_name ^ "_addr")
             |> String.concat ", ") in

        (* TODO: what does 'grid_dim' mean?*)
        grid_init_args_comment ^ "\n" ^
        grid_init_args_emit ^ "\n\n" ^
        grid_init_comment ^ "\n" ^
        "hb_mc_dimension_t grid_dim = {.x = 1, .y = 1};
        hb_mc_dimension_t tg_dim = {.x = MESH_X, .y = MESH_Y};
        err = hb_mc_grid_init(&device, grid_dim, tg_dim, \"%s\", %s, args);
        if (err) return err;" #% fun_name (List.length ds |> string_of_int) 
        |> Core_emit.unindent in

      [input_send_comment;
       input_send_memcpy_emit;
       error_check_emit "hb_mc_memcpy to device failed" ^ "\n";
       grid_init_emit]
      |> String.concat "\n" in

    let run_function_emit : string =
      "// Run the function.
      err = hb_mc_device_tile_groups_execute(&device);
      if (err) return err;" |> Core_emit.unindent in

    let output_receive_emit : string =
      let output_receive_comment : string = Core_emit.unindent
        "// Collect the result by copying \
        output data back over from the device." in

      let output_receive_memcpy_emit : string =
        ds
        |> List.filter (fun d -> d.Ast.data_dir == Ast.Out)
        |> List.map hb_mc_device_memcpy_emit
        |> error_cascade in

      [output_receive_comment;
       output_receive_memcpy_emit;
       error_check_emit "hb_mc_device_memcpy to host failed"]
      |> String.concat "\n" in

    let cleanup_emit : string = 
      let cleanup_comment = "// Clean up." in
      cleanup_comment ^ "\n" ^
      Core_emit.return_emit (Ast.IntExpr 0) in
  
    (* TODO: 32_t in type? *)
    let params : (string * string) list =
      ds
      |> List.map (fun d -> 
          ("*" ^ d.Ast.data_name, 
           (Core_emit.type_emit d.Ast.data_type) ^ "32_t")) in

    Core_emit.fun_emit "int" ("do_" ^ fun_name) params
      ([Core_emit.host_fun_init_emit fun_name;
       eva_init_emit;
       input_send_emit;
       run_function_emit;
       output_receive_emit;
       cleanup_emit;]
      |> String.concat "\n\n") in

  global_constants_emit prog.Ast.target_section prog.Ast.data_section ^
  "\n\n" ^

  header_emit ^ 
  "\n\n" ^

  do_fun_def_emit filename prog.Ast.data_section.ds_data_decls
