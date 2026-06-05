(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)



(* --- *)


(* Position of the first non expanded argument *)
let first_nonexpanded_pos = ref 0

let current = ref (!Arg.current)

let argv = ref Sys.argv

(* Test whether the option is part of a responsefile *)
let is_expanded pos = pos < !first_nonexpanded_pos

let expand_position pos len =
  if pos < !first_nonexpanded_pos then
    (* Shift the position *)
    first_nonexpanded_pos := !first_nonexpanded_pos + len
  else
    (* New last position *)
    first_nonexpanded_pos := pos + len + 2

let input_argument name =
  let filename = Toploop.filename_of_input name in
  let ppf = Format.err_formatter in
  if Filename.check_suffix filename ".cmo"
          || Filename.check_suffix filename ".cma"
  then Toploop.preload_objects := filename :: !Toploop.preload_objects
  else if is_expanded !current then begin
    (* Script files are not allowed in expand options because otherwise the
       check in override arguments may fail since the new argv can be larger
       than the original argv.
    *)
    Printf.eprintf "For implementation reasons, the toplevel does not support\
   \ having script files (here %S) inside expanded arguments passed through the\
   \ -args{,0} command-line option.\n" filename;
    raise (Compenv.Exit_with_status 2)
  end else begin
      let newargs = Array.sub !argv !current
                              (Array.length !argv - !current)
      in
      Compenv.readenv ppf Before_link;
      Compmisc.read_clflags_from_env ();
      if Toploop.prepare ppf ~input:name () &&
         Toploop.run_script ppf name newargs
      then raise (Compenv.Exit_with_status 0)
      else raise (Compenv.Exit_with_status 2)
    end

let file_argument x = input_argument (Toploop.File x)

let wrap_expand f s =
  let start = !current in
  let arr = f s in
  expand_position start (Array.length arr);
  arr

module Options = Main_args.Make_bytetop_options (struct
    include Main_args.Default.Topmain
    let _stdin () = input_argument Toploop.Stdin
    let _args = wrap_expand Arg.read_arg
    let _args0 = wrap_expand Arg.read_arg0
    let anonymous s = file_argument s
    let _eval s = input_argument (Toploop.String  s)
end)

module Custom_loop = struct
open Toploop
open Topcommon

let use_print_results = ref true

let use_silently ppf input =
  Misc.protect_refs
    [ R (use_print_results, false) ]
    (fun () -> use_input ppf input)

let load_explicit_ocamlinit ppf f =
  if Sys.file_exists f then ignore (use_silently ppf (File f) )
  else Format.fprintf ppf "Init file not found: \"%s\".@." f

external windows_xdg_defaults : unit -> string list = "caml_xdg_defaults"

let find_ocamlinit () =
  let ocamlinit = ".ocamlinit" in
  (* 1. .ocamlinit in the current directory *)
  if Sys.file_exists ocamlinit then Some ocamlinit else
  let init_ml = Filename.concat "ocaml" "init.ml" in
  let getenv var = match Sys.getenv_opt var with Some "" -> None | v -> v in
  let is_absolute = Fun.negate Filename.is_relative in
  let exists_in_dir ~file dir =
    let file = Filename.concat dir file in
    if Sys.file_exists file then Some file else None
  in
  let home_dir () = getenv "HOME" in
  let windows_xdg_defaults = Lazy.from_fun windows_xdg_defaults in
  (* 2. ocaml/init.ml under $XDG_CONFIG_HOME (or $HOME/.config on Unix, if
        $XDG_CONFIG_HOME is unset, empty or not an absolute path) *)
  let check_xdg_config_home () =
    match getenv "XDG_CONFIG_HOME" with
    | Some dir when is_absolute dir ->
        exists_in_dir ~file:init_ml dir
    | _ ->
        let default =
          if Sys.win32 then
            (* The first entry of the list is FOLDERID_LocalAppData (exposed by
               default in the process environment as %LOCALAPPDATA%) *)
            match Lazy.force windows_xdg_defaults with
            | dir::_ -> Some dir
            | [] -> None
          else
            Option.map (fun dir -> Filename.concat dir ".config") (home_dir ())
        in
        Option.bind default (exists_in_dir ~file:init_ml)
  in
  (* 3. ocaml/init.ml under any of $XDG_CONFIG_DIRS (or /etc/xdg on Unix, or
        %LOCALAPPDATA%, %APPDATA%, %PROGRAMDATA% on Windows) *)
  let check_xdg_config_dirs () =
    let dirs_from_env =
      match getenv "XDG_CONFIG_DIRS" with
      | Some entry -> List.filter is_absolute (split_path entry)
      | None -> []
    in
    let search =
      if dirs_from_env = [] then
        if Sys.win32 then
          (* There's a non-zero chance that a user of Cygwin, etc. sets
             XDG_CONFIG_HOME for their Cygwin installation and then starts
             native Windows `ocaml.exe` from within that installation. In this
             scenario, XDG_CONFIG_HOME is very unlikely to be a valid path (as
             Cygwin won't have translated it from Unix notation). To mitigate
             this, the default value we take for XDG_CONFIG_DIRS on Windows
             includes the default for XDG_CONFIG_HOME again. If the Cygwin user
             has set both XDG_CONFIG_HOME and XDG_CONFIG_DIRS then we can't help
             them! *)
          Lazy.force windows_xdg_defaults
        else
          ["/etc/xdg"]
      else
        dirs_from_env
    in
    List.find_map (exists_in_dir ~file:init_ml) search
  in
  (* 4. .ocamlinit in $HOME *)
  let check_home () =
    Option.bind (home_dir ()) (exists_in_dir ~file:ocamlinit)
  in
  List.find_map (fun f -> f ())
                [check_xdg_config_home;
                 check_xdg_config_dirs;
                 check_home]

let load_ocamlinit ppf =
  if !Clflags.noinit then ()
  else match !Clflags.init_file with
  | Some f -> load_explicit_ocamlinit ppf f
  | None ->
      match find_ocamlinit () with
      | None -> ()
      | Some file -> ignore (use_silently ppf (File file))


let refill_lexbuf buffer len =
  let open Topcommon in
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !Clflags.noprompt then ""
      else if !first_line then "λ "
      else if !Clflags.nopromptcont then ""
      else if Lexer.in_comment () || !comment_prompt_override then "* "
      else "  "
    in
    first_line := false;
    let (len, eof) = !read_interactive_input prompt buffer len in
    if eof then begin
      Location.echo_eof ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end

(* Without changing the state of [lb], try to see if it contains a token.
   Return [EOF] if there is no token in [lb], a token if there is one,
   or raise a lexer error as appropriate.
   Print lexer warnings or not according to [print_warnings].
*)
let look_ahead ~print_warnings lb =
  let shadow =
    Lexing.{ lb with
      refill_buff = (fun newlb -> newlb.lex_eof_reached <- true);
      lex_buffer = Bytes.copy lb.lex_buffer;
      lex_mem = Array.copy lb.lex_mem;
    }
  in
  Misc.protect_refs [
      R (Lexer.print_warnings, print_warnings);
      Location.(R (report_printer, fun () -> batch_mode_printer));
    ] (fun () -> Lexer.token shadow)
;;



let ends_with_lf lb =
  let open Lexing in
  Bytes.get lb.lex_buffer (lb.lex_buffer_len - 1) = '\n'


(* Refill the buffer until the next linefeed or end-of-file that is not
   inside a comment and check that its contents can be ignored.
   We do this by adding whole lines to the lexbuf until one of these
   occurs:
   - it contains no tokens and no unterminated comments
   - it contains some token or unterminated string
   - it contains a lexical error
*)
let is_blank_with_linefeed lb =
  let open Lexing in
  if Bytes.get lb.lex_buffer lb.lex_curr_pos = '\n' then
    (* shortcut for the most usual case *)
    true
  else begin
    let rec loop () =
      if not (lb.lex_eof_reached || ends_with_lf lb) then begin
        (* Make sure the buffer does not contain a truncated line. *)
        lb.refill_buff lb;
        loop ()
      end else begin
        (* Check for tokens in the lexbuf. We may have to
           repeat this step, so don't print any warnings yet. *)
        match look_ahead ~print_warnings:false lb with
        | EOF -> true (* no tokens *)
        | _ -> false (* some token *)
        | exception Lexer.(Error ((Unterminated_comment _
                                   | Unterminated_string_in_comment _), _)) ->
            (* In this case we don't know whether there will be a token
               before the next linefeed, so get more chars and continue. *)
            Misc.protect_refs [ R (comment_prompt_override, true) ]
              (fun () -> lb.refill_buff lb);
            loop ()
        | exception _ -> false (* syntax error *)
      end
    in
    loop ()
  end

exception PPerror

(* Read and parse toplevel phrases, stop when a complete phrase has been
   parsed and the lexbuf contains and end of line with optional whitespace
   and comments. *)
let rec get_phrases ppf lb phrs =
  match !parse_toplevel_phrase lb with
  | phr ->
    if is_blank_with_linefeed lb then begin
      (* The lexbuf does not contain any tokens. We know it will be
         flushed after the phrases are evaluated, so print warnings now. *)
      ignore (look_ahead ~print_warnings:true lb);
      List.rev (phr :: phrs)
    end else
      get_phrases ppf lb (phr :: phrs)
  | exception Exit -> raise PPerror
  | exception e -> Location.report_exception ppf e; []


(* Type, compile and execute a phrase. *)
let process_phrase ppf snap phr =
  snap := Btype.snapshot ();
  Warnings.reset_fatal ();
  let phr = preprocess_phrase ppf phr in
  Env.reset_cache_toplevel ();
  ignore(execute_phrase true ppf phr)

(* Type, compile and execute a list of phrases, setting the report printer
   to batch mode for all but the first one.
   We have to use batch mode for reporting for two reasons:
   1. we can't underline several parts of the input line(s) in place
   2. the execution of the first phrase may mess up the line count so we
      can't move the cursor back to the correct line
 *)
let process_phrases ppf snap phrs =
  match phrs with
  | [] -> ()
  | phr :: rest ->
    process_phrase ppf snap phr;
    if rest <> [] then begin
      let process ph = Location.reset (); process_phrase ppf snap ph in
      Misc.protect_refs
        Location.[R (report_printer, fun () -> batch_mode_printer)]
        (fun () -> List.iter process rest)
    end


let loop ppf =
  Misc.Style.setup !Clflags.color;
  Clflags.debug := true;
  Location.formatter_for_warnings := ppf;
  if not !Clflags.noversion then
    Format.fprintf ppf "OCaml version %s%s%s@.Enter %a for help.@.@."
      Config.version
      (if Topeval.implementation_label = "" then "" else " - ")
      Topeval.implementation_label
      (Format_doc.compat Misc.Style.inline_code) "#help;;";
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Location.input_phrase_buffer := Some phrase_buffer;
  Sys.catch_break true;
  run_hooks After_setup;
  load_ocamlinit ppf;
  while true do
    let snap = ref (Btype.snapshot ()) in
    try
      Lexing.flush_input lb;
      (* Reset the phrase buffer when we flush the lexing buffer. *)
      Buffer.reset phrase_buffer;
      Location.reset();
      first_line := true;
      let phrs = get_phrases ppf lb [] in
      process_phrases ppf snap phrs
    with
    | End_of_file -> raise (Compenv.Exit_with_status 0)
    | Sys.Break -> Format.fprintf ppf "Interrupted.@."; Btype.backtrack !snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack !snap
  done
end


let main () =
  let ppf = Format.err_formatter in
  let program = "ocaml" in
  let display_deprecated_script_alert =
    Array.length !argv >= 2 && Topcommon.is_command_like_name !argv.(1)
  in
  Topcommon.update_search_path_from_env ();
  Compenv.readenv ppf Before_args;
  if display_deprecated_script_alert then
    Location.deprecated_script_alert program;
  Clflags.add_arguments __LOC__ Options.list;
  Compenv.parse_arguments ~current argv file_argument program;
  Compenv.readenv ppf Before_link;
  Compmisc.read_clflags_from_env ();
  if not (Toploop.prepare ppf ()) then raise (Compenv.Exit_with_status 2);
  Compmisc.init_path ();
  Custom_loop.loop Format.std_formatter

let main () =
  match main () with
  | exception Compenv.Exit_with_status n -> n
  | () -> 0

let () =
  exit (main ())