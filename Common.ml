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

open Bigarray

type bigstr = (char, int8_unsigned_elt, c_layout) Array1.t
type id     = Ident.t
type term   = Lambda.lambda
type value  = Obj.t

type _ ty =
  | TyAny    : 'a ty
  | TyArray  : value array ty
  | TyBigStr : bigstr ty
  | TyBool   : bool ty
  | TyChar   : char ty
  | TyExn    : exn ty
  | TyFloat  : float ty
  | TyInt    : int ty
  | TyInt32  : int32 ty
  | TyInt64  : int64 ty
  | TyLazy   : value Lazy.t ty
  | TyNatInt : nativeint ty
  | TyStr    : string ty
  | TyUnit   : unit ty

let ( @$ ) f x =
  f x

let ( --> ) x y =
  x, y

let ( >> ) g f x = f (g x)

let any    = TyAny
let array  = TyArray
let bigstr = TyBigStr
let bool   = TyBool
let char   = TyChar
let exn    = TyExn
let float  = TyFloat
let int    = TyInt
let int32  = TyInt32
let int64  = TyInt64
let lazy'  = TyLazy
let natint = TyNatInt
let str    = TyStr
let unit   = TyUnit

external bittest : string -> int -> int = "caml_bitvect_test"

external bswap16      : int       -> int       = "caml_bswap16"
external bbswap32     : int32     -> int32     = "caml_int32_bswap"
external bbswap64     : int64     -> int64     = "caml_int64_bswap"
external bbswapnative : nativeint -> nativeint = "caml_nativeint_bswap"

external dup : 'a -> 'a = "caml_obj_dup"

external e_bigstr_get_16 : bigstr -> int -> char = "caml_ba_uint8_get16"
external e_bigstr_get_32 : bigstr -> int -> char = "caml_ba_uint8_get32"
external e_bigstr_get_64 : bigstr -> int -> char = "caml_ba_uint8_get64"

external e_bigstr_set_16 : bigstr -> int -> char -> unit = "caml_ba_uint8_get16"
external e_bigstr_set_32 : bigstr -> int -> char -> unit = "caml_ba_uint8_get32"
external e_bigstr_set_64 : bigstr -> int -> char -> unit = "caml_ba_uint8_get64"

external e_str_get_16 : string -> int -> char = "caml_string_get16"
external e_str_get_32 : string -> int -> char = "caml_string_get32"
external e_str_get_64 : string -> int -> char = "caml_string_get64"

external e_str_set_16 : string -> int -> char -> unit = "caml_string_set16"
external e_str_set_32 : string -> int -> char -> unit = "caml_string_set32"
external e_str_set_64 : string -> int -> char -> unit = "caml_string_set64"

external tamasheq_call_1 : string -> 'a -> 'b
  = "tamasheq_call_1"

external tamasheq_call_2 : string -> 'a -> 'b -> 'c
  = "tamasheq_call_2"

external tamasheq_call_3 : string -> 'a -> 'b -> 'c -> 'd
  = "tamasheq_call_3"

external tamasheq_call_4 : string -> 'a -> 'b -> 'c -> 'd -> 'e
  = "tamasheq_call_4"

external tamasheq_call_5 : string -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
  = "tamasheq_call_5"

external tamasheq_sys_init : string -> string array -> unit
  = "tamasheq_sys_init"

let array_of_queue queue =
  let queue = Queue.copy queue in
  let array = Array.make (Queue.length queue) @$ Queue.peek queue in

  for i = 0 to Array.length array - 1 do
    array.(i) <- Queue.pop queue
  done;

  array

let bigstr_get_n prim unsafe bigstr i =
  if not unsafe && (i < 0 || i >= Array1.dim bigstr) then
      failwith "bigstr_get_n"
  ;

  prim bigstr i

let bigstr_get_16 = bigstr_get_n e_bigstr_get_16
let bigstr_get_32 = bigstr_get_n e_bigstr_get_32
let bigstr_get_64 = bigstr_get_n e_bigstr_get_64

let bigstr_set_n prim unsafe bigstr i v =
  if not unsafe && (i < 0 || i >= Array1.dim bigstr) then
      failwith "bigstr_get_n"
  ;

  prim bigstr i v

let bigstr_set_16 = bigstr_set_n e_bigstr_set_16
let bigstr_set_32 = bigstr_set_n e_bigstr_set_32
let bigstr_set_64 = bigstr_set_n e_bigstr_set_64

let str_get_n prim unsafe str i =
  if not unsafe && (i < 0 || i >= String.length str) then
    failwith "str_get_n"
  ;

  prim str i

let str_get_16 = str_get_n e_str_get_16
let str_get_32 = str_get_n e_str_get_32
let str_get_64 = str_get_n e_str_get_64

let str_set_n prim unsafe str i v =
  if not unsafe && (i < 0 || i >= String.length str) then
    failwith "str_set_n"
  ;

  prim str i v

let str_set_16 = str_set_n e_str_set_16
let str_set_32 = str_set_n e_str_set_32
let str_set_64 = str_set_n e_str_set_64

let sprintf = Printf.sprintf

let module_name =
  Misc.chop_extension_if_any >> Filename.basename >> String.capitalize

let pp = Format.formatter_of_out_channel stdout

let id_not_found id =
  failwith (sprintf "unknown ID ``%s''" (Ident.name id))

let unsupported_arity expected got =
  failwith (sprintf "unsupported arity (expected %d, got %d)" expected got)

let unsupported_lambda_term t =
  failwith "unsupported lambda term"

let cast (type a) (ty : a ty) (v : value) : a =
  Obj.obj v

let lift (type a) (type b)
         ((a, b) : a ty * b ty)
         (f : a -> b) : value list -> value =
  function
  | [x] -> Obj.repr @$ f (cast a x)
  | xs  -> raise @$ unsupported_arity 1 @$ List.length xs

let lift2 (type a) (type b) (type c)
          (((a, b), c) : (a ty * b ty) * c ty)
          (f : a -> b -> c) : value list -> value =
  function
  | [x; y] -> Obj.repr @$ f (cast a x) (cast b y)
  | xs     -> unsupported_arity 3 @$ List.length xs

let lift3 (type a) (type b) (type c) (type d)
          ((((a, b), c), d) : ((a ty * b ty) * c ty) * d ty)
          (f : a -> b -> c -> d) : value list -> value =
  function
  | [x; y; z] -> Obj.repr @$ f (cast a x) (cast b y) (cast c z)
  | xs        -> unsupported_arity 3 @$ List.length xs
