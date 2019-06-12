let emit (prog : Ast.program) : string =
  (* 1. Header *)
  let device_header_emit = 
    let headers = 
      ["\"bsg_manycore.h\""; "\"bsg_set_tile_x_y.h\""] in
    List.map Core_emit.header_emit headers
    |> String.concat "\n" in

  (* 2. constants from: 
    * data section 
    * target section (e.g., xmax)
    * TODO: replace target section with config section 
    *       when groups are implemented*)
  let constants_emit (ds : Ast.data_section) 
                     (ts : Ast.target_section) : string =
    let constant_emit (decl : Ast.typ * string * Ast.expr) : string =
      let (tau, x, e) = decl in
      Core_emit.var_init_emit tau x e in

    let data_section_constants : string list = 
      List.map constant_emit ds.Ast.ds_constant_decls in

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

    data_section_constants @ target_section_constants
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



  (*
  4. Code block function
    Note: we only support up to one code block at the moment, since we don't allow for multiple groups
  *)

  (*
  5. Main declaration
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
  6. Execute code block
  *)

  (*
  7. Send finish and block while waiting

    // each tile sends its own bsg_finish and host takes care of it
    bsg_finish();

    bsg_wait_while(1);
  } *)

  device_header_emit ^
  "\n\n" ^
  constants_emit prog.Ast.data_section prog.Ast.target_section ^
  "\n\n" ^
  array_decls_emit prog.Ast.data_section 

