let indent (n : int) (s : string) : string =
  let rec indentation_builder (n : int) : string = 
    if n == 0 then ""
    else "\t" ^ indentation_builder (n-1) in
  let indentation : string = indentation_builder n in
  let lines : string list = String.split_on_char '\n' s in
  let indented_lines = List.map ((^) (indentation)) lines in
  String.concat "\n" indented_lines

let emit (prog: Ast.program) : string = 
  (* 1. Headers *)
  let host_header_emit : string = 
    let header_files = ["\"f1_helper\""; "<stdlib.h>"] in
    String.concat "\n" (List.map Core_emit.header_emit header_files) in

  (* 2. Constant Macros *)
  let host_tile_size_constants_emit (ts : Ast.target_section) : string = 
    match ts with
    | [] -> ""
    | targ::targs ->
        if List.length targs >= 1
          then failwith Error.unsupported_many_tile_groups_error
          else
            match targ with
            | GlobalMemDecl _ -> ""
            | TileDecl tile ->
              if List.length tile.tile_dims != 2
                then failwith Error.only_two_dims_error
                else
                  let tile_size_consts : (string * Ast.expr) list= 
                    [("X1", IntExpr 0);
                     ("X2", List.nth tile.tile_dims 0);
                     ("Y1", IntExpr 0);
                     ("Y2", List.nth tile.tile_dims 1)] in
                    List.map
                      (fun decl -> 
                        let (x, e) = decl in 
                        Core_emit.expr_macro_emit x e) tile_size_consts
                    |> String.concat "\n" in


  (* 4. Array Decls *)
  let host_data_decls_emit (ds : Ast.data_section) : string =
    let constant_decls_emit : string = 
      let constant_decl_emit 
        (const_decl : (Ast.typ * string * Ast.expr)) =
        match const_decl with
        (typ, x, e) -> Core_emit.var_init_emit typ x e in
      String.concat "" (List.map constant_decl_emit ds.ds_constant_decls) in

    let data_decls_emit : string = 
      let data_decl_emit (d : Ast.data_decl) : string =
        Core_emit.dynamic_alloc_array_emit 
          d.data_type d.data_name d.data_dims in
      String.concat "\n" (List.map data_decl_emit ds.ds_data_decls) in

    constant_decls_emit ^ 
    "// Initialize Arrays" ^
    "\n" ^
    data_decls_emit in

  (* 5. Input Array Layouts *)
  let host_input_data_layouts_emit (ds : Ast.data_section) : string =
    let layout_init_emitted : string =
      if List.exists 
          (fun d -> d.Ast.data_layout == Ast.Blocked) ds.ds_data_decls 
        then Core_emit.host_blocked_layout_init_emit
      else if List.length ds.ds_data_decls == 0 
        then "" 
      else failwith Error.unsupported_layout_error in

    let input_layouts_emitted : string =
      let layout_emit (d : Ast.data_decl) : string =
        match d.data_layout with
        | Blocked -> 
            Core_emit.host_blocked_layout_send_emit 
              d.data_name d.data_type d.data_dir
        | _ -> failwith Error.unsupported_layout_error in
      let input_data_decls =
        List.filter 
          (fun d -> d.Ast.data_dir == Ast.In) ds.ds_data_decls in
      String.concat "\n" (List.map layout_emit input_data_decls) in

    layout_init_emitted ^
    "\n" ^
    input_layouts_emitted in


  (* 6. Start execution of code blocks and wait *)
  let host_execute_blocks_emit = Core_emit.host_execute_code_blocks_emit in

  (* 7. Output Array Decl and Layout *)
  let host_output_data_layouts_emit (ds : Ast.data_section) : string =
    let layout_emit (d : Ast.data_decl) : string =
      match d.data_layout with
      | Blocked -> 
          Core_emit.host_blocked_layout_send_emit 
            d.Ast.data_name 
            d.data_type 
            d.data_dir
      | _ -> failwith Error.unsupported_layout_error in
    let output_data_decls =
      List.filter (fun d -> d.Ast.data_dir == Ast.Out) ds.ds_data_decls in
    String.concat "\n" (List.map layout_emit output_data_decls) in

  (* 8. Do stuff with output *)
  let host_return_emit = Core_emit.return_emit (Ast.IntExpr 0) in

  (* 1. Headers *)
  indent 0 host_header_emit ^ 
  "\n\n" ^

  (* 2. Constant Macros 
   * Currently, target section macros. 
   * Do these need to be macros? Should other variables also be macros?
   * Or should all macros be variables? *)
  indent 0 (host_tile_size_constants_emit prog.target_section) ^ 
  "\n\n" ^

  (* 3. Main declaration and FPGA init: *)
  indent 0 Core_emit.host_main_decl_emit ^ 
  "\n\n" ^

  (* 4. Array Decls *)
  indent 1 (host_data_decls_emit prog.data_section) ^ 
  "\n\n" ^

  (* 5. Input Array Layouts *)
  indent 1 (host_input_data_layouts_emit prog.data_section) ^ 
  "\n\n" ^

  (* 6. Start execution of code blocks and wait *)
  indent 1 host_execute_blocks_emit ^ 
  "\n\n" ^

  (* 7. Output Array Decl and Layout *)
  indent 1 (host_output_data_layouts_emit prog.data_section) ^ 
  "\n\n" ^

  (* 8. Do stuff with output? Or Nothing? *)
  indent 1 host_return_emit ^ "\n}"
