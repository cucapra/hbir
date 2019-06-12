let emit (prog : Ast.program) : string =
  (* 1. Header *)
  let device_header_emit = 
    let headers = 
      ["\"bsg_manycore.h\""; "\"bsg_set_tile_x_y.h\""] in
    List.map Core_emit.header_emit headers
    |> String.concat "\n" in

  (* 2. Macro constants (from data section) *)
  let constants_emit (ds : Ast.data_section) : string =
    let constant_emit (decl : Ast.typ * string * Ast.expr) : string =
      let (tau, x, e) = decl in
      Core_emit.var_init_emit tau x e in
    List.map constant_emit ds.Ast.ds_constant_decls
    |> String.concat "\n" in

  (* 3. Global declaration of inout data arrays *)
  let array_decls_emit (ds : Ast.data_section) : string =
    let array_decl_emit (decl : Ast.data_decl) : string = 
      Core_emit.array_decl_emit 
        decl.Ast.data_type 
        decl.data_name 
        decl.Ast.data_dims in
    List.map array_decl_emit ds.ds_data_decls
    |> String.concat "\n" in


(* 4. Global declaration of implicitly bound vars (e.g., xmax)
    Note: these should come from config, but will come from target while we ignore groups
  *)

  (*
  5. Code block function
    Note: we only support up to one code block at the moment, since we don't allow for multiple groups
  *)

  (*
  6. Main declaration
  int main()
  {
    // Sets the bsg_x and bsg_y global variables.
    bsg_set_tile_x_y();
    int num_tiles = bsg_num_tiles;
    int tile_id   = bsg_x_y_to_id( bsg_x, bsg_y );
    // each tile does the same work for now
    int start_id = 0;
  *)

  (*
  7. Execute code block
  *)

  (*
  8. Send finish and block while waiting

    // each tile sends its own bsg_finish and host takes care of it
    bsg_finish();

    bsg_wait_while(1);
  } *)
  device_header_emit ^
  "\n\n" ^
  constants_emit prog.Ast.data_section^
  "\n\n" ^
  array_decls_emit prog.Ast.data_section 

