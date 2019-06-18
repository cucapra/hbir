(* Compiles F1 Device Program *)
let emit (prog : Ast.program) (filename : string) : string =
  (* 1. Header *)
  let device_header_emit = 
    ["\"bsg_manycore.h\""; 
     "\"bsg_set_tile_x_y.h\"";
     "\"bsg_cuda_lite_runtime.h\"";
     "<stdint.h>"]
    |> List.map Core_emit.header_emit 
    |> String.concat "\n" in

  (* 2. constants from: 
    * data section 
    * target section (e.g., xmax)
    * TODO: replace target section with config section 
    *       when groups are implemented*)
  let constants_emit (ds : Ast.data_section) 
                     (ts : Ast.target_section) : string =
    let constant_emit (decl : Ast.typ * string * Ast.expr) : string =
      let (_, x, e) = decl in
      Core_emit.expr_macro_emit x (Core_emit.expr_emit e) in

    let data_section_constants : string list = 
      List.map constant_emit ds.Ast.ds_constant_decls in

    let layout_constants : string list =
      let num_tiles : string = 
        ts
        |> List.map (fun td ->
            match td with
            | Ast.TileDecl tile -> [tile]
            | _ -> [])
        |> List.flatten
        |> (fun tile_decls -> 
              if List.length tile_decls != 1 
              then failwith Error.exactly_one_tile_group_required
              else tile_decls)
        |> List.hd 
        |> (fun tile -> Core_emit.dims_product_emit tile.tile_dims) in

      let block_size (d : Ast.data_decl) : string = 
        let num_data_elements : string = 
          Core_emit.dims_product_emit d.Ast.data_dims in

        let parens (s : string) : string = "(" ^ s ^ ")" in

        parens num_data_elements ^ "/" ^ parens num_tiles in

      ds.Ast.ds_data_decls
      |> List.map (fun d ->
          ("block_size_" ^ d.Ast.data_name, block_size d))
      |> (@) [("tile_id", "__bsg_id")]
      |> List.map (fun p -> let (x, v) = p in Core_emit.expr_macro_emit x v) in

    (*
    let target_section_constants : string list = 
      match ts with
      | [] -> []
      | targ::targs ->
          if List.length targs >= 1
            then failwith Error.unsupported_many_tile_groups_error
            else
              match targ with
              | GlobalMemDecl _ -> []
              | TileDecl tile ->
                if List.length tile.tile_dims != 2
                  then failwith Error.only_two_dims_error
                  else 
                    List.map 
                     (fun i -> 
                       let var = if i == 0 then "xmax" else "ymax" in
                       constant_emit 
                        (IntTyp, var, List.nth tile.tile_dims i)) 
                     [0; 1] in
    *)

    let completion_barrier_setup : string =
      let macros =
        [("BSG_TILE_GROUP_X_DIM", "bsg_tiles_X");
         ("BSG_TILE_GROUP_Y_DIM", "bsg_tiles_Y")]
        |> List.map (fun p -> let (x, s) = p in Core_emit.expr_macro_emit x s)
        |> String.concat "\n" in

      let barrier_init = Core_emit.unindent
        "INIT_TILE_GROUP_BARRIER(r_barrier, c_barrier, 0, bsg_tiles_X - 1, 0,
     bsg_tiles_Y - 1);" in

      [macros;
       Core_emit.header_emit "\"bsg_tile_group_barrier.h\""; (* This header requires macros first *)
       barrier_init]
      |> String.concat "\n" in

    [data_section_constants |> String.concat "\n"; (*@ target_section_constants *)
     layout_constants |> String.concat "\n";
     completion_barrier_setup]
    |> String.concat "\n\n" in

  (* Global declaration of inout data arrays *)
  (*
  let array_decls_emit (ds : Ast.data_section) : string =
    let array_decl_emit (decl : Ast.data_decl) : string = 
      Core_emit.array_decl_emit 
        decl.Ast.data_type 
        decl.data_name 
        decl.Ast.data_dims in
    List.map array_decl_emit ds.ds_data_decls
    |> String.concat "\n" in
  *)

  let fun_def_emit (fun_name : string) (ds : Ast.data_decl list) (cb : Ast.code_block_decl) : string =
    let ret_type =
      let output_arrays = 
        List.filter (fun d -> d.Ast.data_dir == Ast.Out) ds in

      if List.length output_arrays < 1 
        then failwith Error.no_output_array
        else Core_emit.type_emit (List.hd output_arrays).Ast.data_type in

    let params = 
      (ds
       |> List.map 
            (fun d -> 
              ("*" ^ d.Ast.data_name, 
              (Core_emit.type_emit d.Ast.data_type) ^ "32_t"))
      ) in

    let body = 
      let code_body = Core_emit.stmt_emit cb.Ast.cb_code in
      let barrier_completion = Core_emit.unindent
        "// Barrier to signal completion.
        bsg_tile_group_barrier(&r_barrier, &c_barrier);
        return 0;" in

      [code_body; barrier_completion] |> String.concat "\n" in

    Core_emit.fun_emit ret_type fun_name params body in

  let main_def : string = 
    let body = Core_emit.unindent
      "__wait_until_valid_func();
      return 0;" in

    Core_emit.fun_emit "int" "main" [] body in


  device_header_emit ^
  "\n\n" ^
  constants_emit 
    prog.Ast.data_section prog.Ast.target_section ^
  "\n\n" ^
  fun_def_emit filename prog.Ast.data_section.ds_data_decls (List.hd prog.Ast.code_section.cs_code_block_decls) ^
  "\n\n" ^
  main_def
