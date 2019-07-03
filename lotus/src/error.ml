(* Error Messages *)
let exactly_one_global_mem_required : string =
  "Target Section Error: exactly one global memory required."
let exactly_one_tile_group_required : string =
  "Target Section Error: " ^
  "exactly one tile group required."
let only_two_dims_error : string =
  "Target Section Error: " ^ 
  "only 2D tile groups are supported atm."
let unsupported_layout_error : string =
  "Data Section Error: only Blocked layout supported."
let no_output_array : string =
  "Data Section Error: at least one output array must be declared."
let unsupported_multiple_code_blocks : string =
  "Code Section Error: only up to 1 code block supported."
let unsupported_abstract_iteration (n : int) : string =
  "Code Section Error: abstract iteration unsupported for " ^ string_of_int n ^ " dimensional tensor."
