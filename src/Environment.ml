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

type t = {
  global  : (id, value)      Hashtbl.t;
  lexical : (id * value ref) list;
}

let assign_lexical env id v =
  List.assoc id env.lexical := v

let bind_global env id v =
  Hashtbl.add env.global id v

let bind_lexical env id v =
  { env with lexical = (id, ref v) :: env.lexical }

let bind_lexical_all env l =
  List.fold_left (fun env (id, v) -> bind_lexical env id v) env l

let fresh () =
  { global = Hashtbl.create 10; lexical = []; }

let lookup_global env id =
  try  Hashtbl.find env.global id
  with Not_found ->
    try  Symtable.get_global_value id
    with Symtable.Error e -> id_not_found id

let lookup_lexical env id =
  try  !(List.assoc id env.lexical)
  with Not_found -> id_not_found id

let lookup_lexical_str env id =
  !(snd @$ List.find (fun (id', _) -> id = id'.Ident.name) env.lexical)
