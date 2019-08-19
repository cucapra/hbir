open Vg
open Gg


let generate_arrangement_image _ =

  let draw_rectangle (color: color) 
                     (label : string) 
                     (grid_rows : int)
                     (row_range : int * int)
                     (col_range : int * int)
                     : Vg.image =

  let (min_row, max_row), (min_col, max_col) = row_range, col_range in
  let num_rows = max_row - min_row + 1 in
  let num_cols = max_col - min_col + 1 in

  let rect_image : Vg.image =
    let rect_path = P.empty |> P.rect Box2.unit in
    let rect_outline = `O { P.o with P.width = 0.008 } in
    let outline_color : Vg.image = I.const Color.black in
    I.blend
    (I.cut ~area:rect_outline rect_path outline_color)
    (I.const color) 
    |> I.cut rect_path 
    |> I.scale 
        (V2.v 
          (float_of_int num_cols)
          (float_of_int num_rows)
        ) in

  let font_image : Vg.image =
    let open_sans_xbold =
      { Font.name = "Open Sans";
        size = 1.0;
        weight = `W500;
        slant = `Normal} in
    let font =
      { open_sans_xbold with
        Font.size = Size2.w Size2.unit /. 6.} in
    I.cut_glyphs ~text:label font [] 
      (I.const (Color.blend color (Color.with_a Color.black 0.8)))
    |> I.move (V2.v 0.3 (float_of_int (num_rows-1) +. 0.4)) in

  I.blend font_image rect_image
  |> I.move 
     (V2.v (float_of_int min_col) 
           (float_of_int (grid_rows - num_rows - min_row))) in


  let draw_grid (size : int * int) : Vg.image =
    let rows, cols = size in

    (* maps tile index to spatial position as follows:
      * [ (0,0) (0,1) ... (0,m)
      *   ..
      *   (n,0) (n,1) ... (n,m) ] *)
    let add_tile_at_index (row: int) (col: int) (image : Vg.image) : Vg.image =
      let tile_label = 
        Printf.sprintf "%d x %d" row col in
      let tile : Vg.image = 
        draw_rectangle (Color.gray 0.5) tile_label rows (row, row) (col, col) in
      I.blend tile image in

    let rec build_grid row col image =
      let grid_accum = add_tile_at_index row col image in
      match row, col with
      | 0, 0 -> grid_accum
      | _, 0 -> grid_accum |> build_grid (row-1) (cols-1)
      | _ -> grid_accum |> build_grid row (col-1) in

    build_grid (rows-1) (cols-1) I.void in

(*
  let draw_abs_arrangement (arr : Group.abs_arrangement) : Vg.image =
    let draw_group_array (ga : Group.group_array) : Vg.image =
      
    and draw_group (ix_g : Group.ga_index * Group.abs_group) (color : Vg.color) : Vg.image = 
      let ix, g = ix_g in
      let group_name = Printf.sprintf "%s%s" g.g_rel_name (Group.ga_index_to_string ix) in
      draw_rectangle color group_name 
      in
      
    let grid_image : Vg.image = draw_grid arr.abs_arr_grid_size in
    let groups : Vg.image list = List.map draw_group_array arr.abs_arr_groups in
*)

  let write_image (r_image : Vgr.renderable) : unit =
    try
      print_endline "creating image file...";
      let oc = open_out "image.svg" in
      let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
      try
        ignore (Vgr.render r (`Image r_image));
        ignore (Vgr.render r `End);
        close_out oc
      with 
        e -> 
          print_endline "error...";
          close_out oc; raise e
    with Sys_error e -> prerr_endline e in

  let rows, cols = (4, 4) in
  let view_length = max rows cols in
  write_image 
    ( Size2.v 200. 200., 
      Box2.v (V2.v 0. 0.) (V2.v (float_of_int view_length) (float_of_int view_length)),
      I.blend 
      (draw_rectangle (Color.with_a Color.green 0.5) "tile" 4 (0, 1) (0, 3))
      (draw_grid (4, 4))
    )


(*
type arrangement = (int * int) * (group list)

let generate_arrangement_image (_: Ast.program) : unit =


  let draw_tile_grid (size : int * int)
                     (origin : int * int) (*only affects tile label, not grid pos*)
                     (color : color) : Vg.image =
    let rows, cols = size in

    (* maps tile index to spatial position as follows:
      * [ (0,0) (0,1) ... (0,m)
      *   ..
      *   (n,0) (n,1) ... (n,m) ] *)
    let add_tile_at_index (row: int) (col: int) (image : Vg.image) : Vg.image =
      let row_origin, col_origin = origin in
      let tile_label = Printf.sprintf "%d x %d" (row + row_origin) (col + col_origin) in
      let tile : Vg.image = draw_tile color tile_label in
      let moved_tile = 
        I.move (V2.v (float col) (float (rows-1 - row))) tile in
      I.blend moved_tile image in

    let rec build_grid row col image =
      let grid_accum = add_tile_at_index row col image in
      match row, col with
      | 0, 0 -> grid_accum
      | _, 0 -> grid_accum |> build_grid (row-1) (cols-1)
      | _ -> grid_accum |> build_grid row (col-1) in

    build_grid (rows-1) (cols-1) I.void in

  let draw_group (g : group) : Vg.image =
    let rec draw_sub_group (sg : sub_group) : Vg.image =
      let (parent_rows, _) = sg.parent_size in

      let group_rows, _ = g.group_size in
      let row_start, col_start = sg.origin in

      let x_pos = float (col_start) in
      let y_pos = float (parent_rows - group_rows - row_start) in

      draw_tile_grid g.group_size sg.origin g.group_color
      |> I.move (V2.v x_pos y_pos) in
    
    if List.length g.sub_groups > 0
    then 
      List.map draw_sub_group g.sub_groups
      |> List.fold_left I.blend I.void
    else
      draw_sub_group 
      {group = g;
       origin = (0,0);
       parent_size = (g.group_size); } in

  let test_group =
    let tile_grid_size = (4, 4) in
    arrangement tile_grid_size
      [ let parent_group_size = tile_grid_size in
        let group_size = (2, 4) in
        let group_origin = (0, 0) in 
          group parent_group_size group_size group_origin
          ( let parent_group_size = group_size in
            let array_group_dims = (1, 4) in
            let group_array_origin = (0, 0) in
            group_array parent_group_size array_group_dims group_array_origin)] in

  let group_array (array_size : int * int)
                  (parent : group) : group =
    let parent_rows, parent_cols = parent.group_size in
    let dim1, dim2 = array_size in
    let sub_group_rows, sub_group_cols = parent_rows/dim1, parent_cols/dim2 in

    let group_origins : (int * int) list = 
      let rec range n accum : int list =
        if n > 0 then (range (n-1) ([n]@accum)) else accum in
      let row_range = range dim1 [] in
      let col_range = range dim2 [] in
      print_endline (Printf.sprintf "row_range size: %d" (List.length row_range));
      print_endline (Printf.sprintf "col_range size: %d" (List.length col_range));
      List.map
      (fun row ->
        List.map
        (fun col -> (row, col))
        col_range)
      row_range
      |> List.concat in

    let sub_group_color : color = 
      Color.v (Color.r parent.group_color *. 1.3)
              (Color.g parent.group_color *. 1.3)
              (Color.b parent.group_color *. 1.3)
              (Color.a parent.group_color *. 1.3) in

    let array_sub_groups : group list =
      group_origins |>
      List.map (fun o ->

        { group_size = (sub_group_rows, sub_group_cols);
          group_color = sub_group_color;
          sub_groups = []; }) in
    { group_size = parent.group_size;
      group_origin = parent.group_origin;
      group_color = parent.group_color;
      sub_groups = array_sub_groups; } in

  let faint_blue = Color.v 0. 0. 1. 0.2 in
  let faint_green = Color.v 0. 1. 0. 0.2 in
  let test_arrangement1 = 
    {arr_size = (4, 4);
     arr_groups = [
       group_array (1, 4)
       { group_size = (2, 4);
         group_origin = (0, 0);
         group_color = faint_blue;
         sub_groups = [] };
       { group_size = (2, 4);
         group_origin = (2, 0);
         group_color = faint_green;
         sub_groups = [] }
     ]} in

  let test_arrangement2 = 
    {arr_size = (4, 4);
     arr_groups = [
       ( group_array (1, 1)
       { group_size = (4, 4);
         group_origin = (0, 0);
         group_color = faint_blue;
         sub_groups = []});
     ]} in

  let _ =
    let g = 
      group_array (4, 4)
      { group_size = (4, 4);
        group_origin = (0, 0);
        group_color = faint_blue;
        sub_groups = []} in
    print_endline (Printf.sprintf "# subgroups = %d" (List.length g.sub_groups));
    List.map
    (fun g ->
      let (row, col) = g.group_origin in
      print_endline (Printf.sprintf "origin at: (%d, %d)" row col))
    g.sub_groups in

  let _ = test_arrangement1 in
  let _ = test_arrangement2 in
*)
