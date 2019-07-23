open Vg
open Gg

type group = {
  group_size : int * int;
  group_origin : int * int;
  group_color : color;
  sub_groups : group list;
}

type tile_arrangement = {
  arr_size : int * int;
  arr_groups : group list;
}

let generate_arrangement_image (_: Ast.program) : unit =

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

  let draw_tile (color: color) (tile_label : string) : Vg.image =
    let tile_image : Vg.image =
      let tile_path = P.empty |> P.rect Box2.unit in
      let tile_outline = `O { P.o with P.width = 0.02 } in
      let outline_color : Vg.image = I.const Color.black in
      I.blend
      (I.cut ~area:tile_outline tile_path outline_color)
      (I.const color) 
      |> I.cut tile_path in

    let font_image : Vg.image =
      let open_sans_xbold =
        { Font.name = "Open Sans";
          size = 1.0;
          weight = `W500;
          slant = `Normal} in
      let font =
        { open_sans_xbold with
          Font.size = Size2.w Size2.unit /. 6.} in
      I.cut_glyphs ~text:tile_label font [] (I.const Color.black) 
      |> I.move (V2.v 0.3 0.4) in

    I.blend font_image tile_image in


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


  let draw_arrangement (arr : tile_arrangement) : Vg.image =
    let base_img : Vg.image = 
      draw_tile_grid arr.arr_size (0, 0) Color.void in

    let rec draw_group (parent_size : int * int)
                       (g : group) : Vg.image =
      let group_img : Vg.image =
        let parent_rows, _ = parent_size in

        let group_rows, _ = g.group_size in
        let row_start, col_start = g.group_origin in

        let x_pos = float (col_start) in
        let y_pos = float (parent_rows - group_rows - row_start) in

        draw_tile_grid g.group_size g.group_origin g.group_color
        |> I.move (V2.v x_pos y_pos) in

      List.map (draw_group g.group_size) g.sub_groups
      |> List.fold_left I.blend group_img in

    List.map (draw_group arr.arr_size) arr.arr_groups
    |> List.fold_left I.blend base_img in


  let test_arrangement = 
    let faint_blue = Color.v 0. 0. 1. 0.2 in
    let faint_green = Color.v 0. 1. 0. 0.2 in
    {arr_size = (4, 4);
     arr_groups = [
       { group_size = (2, 4);
         group_origin = (0, 0);
         group_color = faint_blue;
         sub_groups = [] };
       { group_size = (2, 4);
         group_origin = (2, 0);
         group_color = faint_green;
         sub_groups = [] }
     ]} in

  write_image (Size2.v 200. 200., Box2.v (V2.v 0. 0.) (Size2.v 4. 4.), draw_arrangement test_arrangement)


