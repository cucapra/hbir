open Langstart
open Ast

let program_file : string option ref = ref None

let set_program_file (arg : string) : unit =
    match !program_file with
    | None -> program_file := Some arg
    | Some _ -> ()  (* Don't overwrite program_file *)

let run_gcc : bool ref = ref false
let set_gcc () : unit = run_gcc := true

let run_bsg : bool ref = ref false
let set_bsg () : unit = run_bsg := true

let run_pp : bool ref = ref false
let set_pp () : unit = run_pp := true

let run_v : bool ref = ref false
let set_v () : unit = run_pp := true

let usage = "HBIR compiler\n"
let spec : (Arg.key * Arg.spec * Arg.doc) list =
    [("-pp", Arg.Set run_pp,
    "Runs the given file with the pretty printer (replaces standard output)");
     ("-gcc", Arg.Set run_gcc,
    "Generates code that can be compiled by gcc");
    ("-bsg", Arg.Set run_bsg,
    "Generates code and a Makefile that can be run on the Manycore RTL sim or F1 instance");
    ("-v", Arg.Set run_v,
    "Prints contents in emitted files")
    ]

let prog =
    Arg.parse spec set_program_file usage;
        match !program_file with
        None -> print_string (Arg.usage_string spec usage) | Some f ->
    let ch =
        try open_in f
        with Sys_error s -> failwith ("Cannot open file: " ^ s) in
    let prog : program =
        let lexbuf = Lexing.from_channel ch in
        try
            Parser.main Lexer.token lexbuf
        with
            | _ ->
                begin
                    close_in ch;
                let pos = lexbuf.Lexing.lex_curr_p in
                let tok = (Lexing.lexeme lexbuf) in
                (* let line = pos.Lexing.pos_lnum in *)
                let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
                failwith ("Parsing error at token '" ^ tok ^ "', line "
                     ^ (string_of_int pos.Lexing.pos_lnum) ^ ", column " ^ string_of_int cnum)
                end in
        close_in ch;
    if !run_pp then print_endline (Ops.pretty_program prog);
    (* TODO: Should create a new directory with main.c and Makefile to mirror bsg_manycore programsg *)
    if !run_bsg then
        let ch1 = open_out (*f ^*) "device.c" in
        output_string ch1 (Manycore.convert_ast prog);
        close_out ch1;
        let ch2 = open_out "Makefile" in
        output_string ch2 (Manycore.generate_makefile prog);
        close_out ch2;
        (* PBB: generate host program *)
        let ch3 = open_out (*f ^*) "host.c" in
        output_string ch3 (Manycore.convert_ast prog);
        close_out ch3;
        print_endline (Manycore.convert_ast prog);
        print_endline (Manycore.convert_ast prog);
        print_endline (Manycore.generate_makefile prog);
        if !run_v then print_endline (Manycore.convert_ast prog);
    if !run_gcc then
        let ch = open_out (*f ^*) "main.c" in
        output_string ch (Simplec.convert_ast prog);
        close_out ch;
        if !run_v then print_endline (Simplec.convert_ast prog);