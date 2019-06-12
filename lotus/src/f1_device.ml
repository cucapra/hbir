let emit (_: Ast.program) : string =
  (* 1) Header *)
  let device_header = 
    let headers = 
      ["\"bsg_manycore.h\""; "\"bsg_set_tile_x_y.h\""] in
    List.map Core_emit.header_emit headers
    |> String.concat "\n" in

  (*
  2) Macro constants (from data section)
  *)

  (*
  3) Global declaration of inout data arrays
  *)

  (*
  4) Global declaration of implicitly bound vars (e.g., xmax)
    Note: these should come from config, but will come from target while we ignore groups
  *)

  (*
  5) Code block function
    Note: we only support up to one code block at the moment, since we don't allow for multiple groups
  *)

  (*
  6) Main declaration
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
  7) Execute code block
  *)

  (*
  8) Send finish and block while waiting

    // each tile sends its own bsg_finish and host takes care of it
    bsg_finish();

    bsg_wait_while(1);
  } *)
  device_header

