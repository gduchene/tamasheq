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

open Common
open Printf

type env          = Environment.t
and  finalize_fun = env -> result
and  hook         = string * init_fun * finalize_fun
and  init_fun     = term list -> result
and  result       = [ `Abort of string option
                    | `Continue
                    | `Warn of string option ]
and  term         = Lambda.lambda

let hooks : hook Queue.t = Queue.create ()

let abort hook_name hook_reason =
  match hook_reason with
  | Some reason -> eprintf "fatal error: hook %s: %s" hook_name reason
  | None        -> eprintf "fatal error: hook %s failed" hook_name
  ;

  exit 1

let warn hook_name hook_reason =
  match hook_reason with
  | Some reason -> eprintf "warning: hook %s: %s" hook_name reason
  | None        -> eprintf "warning: hook %s failed" hook_name

let handle_result hook_name = function
  | `Abort why -> abort hook_name why
  | `Continue  -> ()
  | `Warn why  -> warn hook_name why

let init_hooks terms =
  Queue.iter (fun (n, f, _) -> handle_result n @$ f terms) hooks

let finalize_hooks env =
  Queue.iter (fun (n, _, f) -> handle_result n @$ f env) hooks

let register_hook hook = Queue.push hook hooks
