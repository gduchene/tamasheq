(*
Copyright (c) 2014, Grégoire Duchêne <gduchene@awhk.org>

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.
*)

open Arg
open Common
open Interpreter

let argsk  = Queue.create ()
let denv   = ref false
let dlam   = ref false
let files  = Queue.create ()
let hooks  = Queue.create ()
let intfs  = Queue.create ()
let outdir = ref "."
let test   = ref false

let load_mli filename outdir =
  let open Pparse    in
  let open Typedtree in

  Location.input_name := filename;
  Compmisc.init_path false;
  Clflags.debug := true;

  let input = preprocess filename  in
  let mname = module_name filename in

  Env.set_unit_name mname;

  let env = Compmisc.initial_env () in

  try
    let tsg =
      Typemod.transl_signature env @$
        file pp input Parse.interface Config.ast_intf_magic_number
    in

    Typecore.force_delayed_checks ();

    let output = outdir ^ "/" ^ mname in
    let sg     = Env.save_signature tsg.sig_type mname (output ^ ".cmi") in

    ignore @$ Typemod.save_signature mname tsg output filename env sg
  with
  | Env.Error msg ->
     Env.report_error pp msg;
     failwith "fatal error"

  | Typetexp.Error (_, _, msg) ->
     Typetexp.report_error env pp msg;
     failwith "fatal error"

let load_ml filename outdir =
  let open Config in
  let open Pparse in

  Location.input_name := filename;
  Compmisc.init_path false;
  Clflags.debug := true;

  let input = preprocess filename  in
  let mname = module_name filename in

  Env.set_unit_name mname;
  Typecore.reset_delayed_checks ();

  let env = Compmisc.initial_env () in

  try
    let filename = (Filename.chop_extension filename) ^ ".ml" in

    file pp input Parse.implementation ast_intf_magic_number
    |> Typemod.type_implementation filename (outdir ^ "/" ^ mname) mname env
    |> Translmod.transl_implementation mname
    |> Simplif.simplify_lambda
  with
  | Env.Error msg ->
     Env.report_error pp msg;
     failwith "fatal error"

  | Typetexp.Error (_, _, msg) ->
     Typetexp.report_error env pp msg;
     failwith "fatal error"

let load_mlis filenames outdir =
  Queue.iter (fun filename -> load_mli filename outdir) filenames

let load_mls filenames outdir =
  List.rev @$ Queue.fold (fun acc s -> load_ml s outdir :: acc) [] filenames

let () =
  let args = [
    "-denv", Unit   (fun () -> denv := not (!denv)), " Print the environment";
    "-dlam", Unit   (fun () -> denv := not (!dlam)), " Print the Lambda AST";
    "-h"   , String (fun s  -> Queue.add s hooks)  , "<hook> Run <hook>";
    "-o"   , String (fun s  -> outdir := s)        , "<dir> Output files in <dir>";
    "-t"   , Unit   (fun s  -> test := not (!test)), " Always exit successfully";
    "--"   , Rest   (fun s  -> Queue.add s argsk)  , " (undocumented)";
  ]
  in
  let anon_arg s =
    if Filename.check_suffix s "cma" || Filename.check_suffix s "cmo" then
      try  ignore (Topdirs.load_file Format.err_formatter s)
      with Lexer.Error (msg, _) -> Lexer.report_error pp msg
    else
      let mli = (Filename.chop_extension s) ^ ".mli" in

      if Sys.file_exists mli then
        Queue.add mli intfs
      ;

      Queue.add s files
  in

  parse (align args) anon_arg "Usage:";
  Clflags.include_dirs := !outdir :: !Clflags.include_dirs;

  if Queue.is_empty files then
    begin
      prerr_endline "you must specify at last one .ml file";
      usage (align args) "Usage:"
    end
  else
    begin
      load_mlis intfs !outdir;

      let env    = Environment.fresh ()   in
      let lambda = load_mls files !outdir in

      if !dlam then
        List.iter (Printlambda.lambda pp >> Format.print_newline) lambda
      ;

      try
        begin
          List.iter (fun l -> ignore @$ eval env [] l) lambda;

          if !denv then
            Environment.dump env stderr
        end
      with exn ->
        if !test then
          prerr_endline @$ Printexc.to_string exn
        else
          raise exn
    end
