open Lotuslib
open Ast

let program_file : string option ref = ref None

let set_program_file (arg : string) : unit =
    match !program_file with
    | None -> program_file := Some arg
    | Some _ -> ()  (* Don't overwrite program_file *)

let run_gcc : bool ref = ref false
let set_gcc () : unit = run_gcc := true


let run_f1_wrapper : bool ref = ref false
let set_f1_wrapper () : unit = run_f1_wrapper := true

let run_bsg : bool ref = ref false
let set_bsg () : unit = run_bsg := true
let write_bsg (prog : program) : unit =
    let out_dir : string = "sim-gen" in
    let _ = Sys.command ("mkdir -p " ^ out_dir) in
    let ch1 = open_out (*f ^*) (out_dir ^ "/main.c") in
    output_string ch1 (Manycore.convert_ast prog);
    close_out ch1;
    let ch2 = open_out (out_dir ^ "/Makefile") in
    output_string ch2 (Manycore.generate_makefile prog);
    close_out ch2;
    print_endline (Manycore.convert_ast prog);
    print_endline (Manycore.generate_makefile prog)

let run_f1 : bool ref = ref false
let write_f1 (prog : program) (filename : string) : unit =
    let out_dir : string = "." in
    let _ = Sys.command ("mkdir -p " ^ out_dir) in
    let ch1 = open_out (out_dir ^ "/" ^ filename ^ ".c") in
    output_string ch1 (F1_device.emit prog filename);
    close_out ch1;
    let ch2 = open_out (out_dir ^ "/Makefile") in
    output_string ch2 ("");
    close_out ch2;
    (* PBB: generate host program *)
    let ch3 = open_out (out_dir ^ "/host.c") in
    output_string ch3 (F1_host.emit prog filename);
    close_out ch3;
    ()

let run_pp : bool ref = ref false
let set_pp () : unit = run_pp := true

let run_v : bool ref = ref false
let set_v () : unit = run_pp := true

let run_visualize : bool ref = ref false
let set_visualize () : unit = run_visualize := true

let usage = "HBIR compiler\n"
let spec : (Arg.key * Arg.spec * Arg.doc) list =
    [("-pp", Arg.Set run_pp,
    "Runs the given file with the pretty printer (replaces standard output)");
     ("-gcc", Arg.Set run_gcc,
    "Generates code that can be compiled by gcc");
    ("-bsg", Arg.Set run_bsg,
    "Generates code and a Makefile that can be run on the Manycore RTL sim");
    ("-f1", Arg.Set run_f1,
        "Generates code and a Makefile that can be run on the F1 instance");
    ("-visualize", Arg.Set run_visualize, 
        "Generates an image representing the tile arrangement");
    (* where the data should come from *)
    ("-wrapper", Arg.Set run_f1_wrapper,
        "Give c function and header to be initiated from another file");
    ("-v", Arg.Set run_v,
    "Prints contents in emitted files")
    ]

let prog =
    Arg.parse spec set_program_file usage;
    match !program_file with
    | None -> print_string (Arg.usage_string spec usage) 
    | Some f ->
      let ch =
        try open_in f
        with Sys_error s -> failwith ("Cannot open file: " ^ s) in
        let prog : program =
          let lexbuf = Lexing.from_channel ch in
          try
              Parser.main Lexer.token lexbuf
          with
              | _ ->
                close_in ch;
                let pos = lexbuf.Lexing.lex_curr_p in
                let tok = (Lexing.lexeme lexbuf) in
                let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
                failwith ("Parsing error at token '" ^ tok ^ "', line "
                 ^ (string_of_int pos.Lexing.pos_lnum) ^ ", column " ^ string_of_int cnum)
                in
      close_in ch;
    if !run_f1 then 
      begin
        let filename : string = 
          String.split_on_char '/' f
          |> List.rev
          |> List.hd 
          |> String.split_on_char '.' 
          |> List.hd in
        write_f1 prog filename 
      end;
    if !run_visualize then 
      begin
        Group.print_config_section prog ;
        Visualize.generate_arrangement_image prog;
      end
    (*
    if !run_pp then print_endline (Ops.pretty_program prog);
    if !run_bsg then write_bsg prog;
    if !run_gcc then
        let out_dir : string = "gcc-gen" in
        let _ = Sys.command ("mkdir -p " ^ out_dir) in
        let ch = open_out (*f ^*) (out_dir ^ "main.c") in
        output_string ch (Simplec.convert_ast prog);
        close_out ch;
    *)
        (* if !run_v then print_endline (Simplec.convert_ast prog); *)
