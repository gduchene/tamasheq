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

open Asttypes
open Bigarray
open Common
open Environment
open Lambda
open Printf

let nil = Obj.repr ()

let rec apply_fun f args =
  let f = ((Obj.obj f) : 'a -> 'b) in

  match args with
  | []      -> assert false
  | [x]     -> Obj.repr @$ f x
  | x :: xs -> apply_fun (Obj.repr @$ f x) xs

let rec eval env ks t =
  match t with
  | Lapply (body, args, loc) -> eval_apply env ks body args loc

  | Lassign (id, t) ->
     assign_lexical env id @$ eval env ks t;
     nil

  | Lconst const -> eval_const const

  | Levent (t, _) -> eval env ks t

  | Lfor (index, low, high, dir, body) ->
     eval_for env ks index low high dir body

  | Lfunction   (kind, ids, body)  -> eval_fun env ks kind ids body
  | Lifthenelse (pred, succ, fail) -> eval_if env ks pred succ fail
  | Lifused     _                  -> unsupported_lambda_term t

  | Llet (_, id, def, body) ->
     eval (bind_lexical env id @$ eval env ks def) ks body

  | Lletrec (defs, body) ->
     eval_let_rec env ks defs body

  (* special case for predefined exceptions *)
  | Lprim (Pmakeblock (tag, _),
           (Lprim (Pgetglobal id, []) :: args)) when Ident.global id ->
     let args = eval_all env ks args in

     begin
       match Ident.name id with
       | "Assert_failure" ->
          let f s i j = Assert_failure (s, i, j) in

          lift3 (str --> int --> int --> exn) f args

       | "Division_by_zero" -> Obj.repr Division_by_zero
       | "End_of_file"      -> Obj.repr End_of_file

       | "Failure" ->
          let f s = Failure s in

          lift (str --> exn) f args

       | "Invalid_arg" ->
          let f s = Invalid_argument s in

          lift (str --> exn) f args

       | "Match_failure" ->
          let f s i j = Match_failure (s, i, j) in

          lift3 (str --> int --> int --> exn) f args

       | "Not_found"      -> Obj.repr Not_found
       | "Stack_overflow" -> Obj.repr Stack_overflow
       | "Sys_blocked_io" -> Obj.repr Sys_blocked_io

       | "Sys_error" ->
          let f s = Sys_error s in

          lift (str --> exn) f args

       | "Undefined_recursive_module" ->
          let f s i j = Undefined_recursive_module (s, i, j) in

          lift3 (str --> int --> int --> exn) f args

       | _ ->
          failwith @$ sprintf "unknown global ``%s''!" @$ Ident.name id
     end

  | Lprim (kind, ts) -> eval_prim env kind @$ eval_all env ks ts

  | Lsequence (fst, snd) ->
     ignore @$ eval env ks fst;
     eval env ks snd

  | Lsend _  -> unsupported_lambda_term t

  | Lstaticcatch (body, (tag, ids), hdl) -> eval_catch env ks body tag ids hdl
  | Lstaticraise (tag, args)             -> eval_raise ks tag args

  | Lswitch  (body, switch)  -> eval_switch env ks body switch
  | Ltrywith (body, id, hdl) -> eval_try_with env ks body id hdl
  | Lvar     id              -> lookup_lexical env id
  | Lwhile   (pred, body)    -> eval_while env ks pred body

and eval_all env ks ts =
  List.map (eval env ks) ts

and eval_apply env ks body args loc =
  apply_fun (eval env ks body) @$ eval_all env ks args

and eval_catch env ks body tag ids k =
  eval env ((tag, eval_fun env ks Curried ids k) :: ks) body

and eval_const = function
  | Const_base (Const_int       x) -> Obj.repr x
  | Const_base (Const_char      x) -> Obj.repr x
  | Const_base (Const_float     x) -> Obj.repr x
  | Const_base (Const_int32     x) -> Obj.repr x
  | Const_base (Const_int64     x) -> Obj.repr x
  | Const_base (Const_string    x) -> Obj.repr x
  | Const_base (Const_nativeint x) -> Obj.repr x

  | Const_block (tag, cs) ->
     let blk = Obj.new_block tag @$ List.length cs in

     List.iteri (fun i c -> Obj.set_field blk i @$ eval_const c) cs;
     blk

  | Const_float_array fs ->
     Obj.repr @$ Array.of_list @$ List.map float_of_string fs

  | Const_immstring x -> Obj.repr x
  | Const_pointer   x -> Obj.repr x

and eval_for env ks index low high dir body =
  let low  = cast TyInt @$ eval env ks low  in
  let high = cast TyInt @$ eval env ks high in

  match dir with
  | Asttypes.Upto ->
     for x = low to high do
       ignore @$ eval (bind_lexical env index @$ Obj.repr x) ks body
     done;

     nil

  | Asttypes.Downto ->
     for x = low downto high do
       ignore @$ eval (bind_lexical env index @$ Obj.repr x) ks body
     done;

     nil

and eval_fun env ks kind ids body =
  Obj.repr @$
    match ids with
    | []        -> assert false
    | [id]      -> fun x -> eval (bind_lexical env id x) ks body
    | id :: ids -> fun x -> eval_fun (bind_lexical env id x) ks kind ids body

and eval_if env ks pred succ fail =
  let next =
    match cast TyInt @$ eval env ks pred with
    | 0 -> fail
    | _ -> succ
  in

  eval env ks next

and eval_let_rec env ks defs body =
  let nils         = List.map (fun (id, _) -> id, nil) defs in
  let env          = bind_lexical_all env nils in
  let f (id, body) = assign_lexical env id @$ eval env ks body in

  List.iter f defs;
  eval env ks body

and eval_prim env kind vs =
  match kind with
  | Pbittest -> lift2 (str --> int --> int) bittest vs

  | Pbigarraydim n -> lift (any --> int) (fun a -> Genarray.nth_dim a n) vs

  | Pbbswap  Pint32     -> lift (int32  --> int32 ) bbswap32     vs
  | Pbbswap  Pint64     -> lift (int64  --> int64 ) bbswap64     vs
  | Pbbswap  Pnativeint -> lift (natint --> natint) bbswapnative vs
  | Pbswap16            -> lift (int    --> int   ) bswap16      vs

  | Pnegint  -> lift  (int --> int)            ( ~-   ) vs
  | Paddint  -> lift2 (int --> int --> int)    ( +    ) vs
  | Psubint  -> lift2 (int --> int --> int)    ( -    ) vs
  | Pmulint  -> lift2 (int --> int --> int)    ( *    ) vs
  | Pdivint  -> lift2 (int --> int --> int)    ( /    ) vs
  | Pmodint  -> lift2 (int --> int --> int)    ( mod  ) vs
  | Pandint  -> lift2 (int --> int --> int)    ( land ) vs
  | Porint   -> lift2 (int --> int --> int)    ( lor  ) vs
  | Pxorint  -> lift2 (int --> int --> int)    ( lxor ) vs
  | Plslint  -> lift2 (int --> int --> int)    ( lsl  ) vs
  | Plsrint  -> lift2 (int --> int --> int)    ( lsr  ) vs
  | Pasrint  -> lift2 (int --> int --> int)    ( asr  ) vs
  | Psequand -> lift2 (bool --> bool --> bool) ( && )   vs
  | Psequor  -> lift2 (bool --> bool --> bool) ( || )   vs

  | Pintcomp    Ceq    -> lift2 (int --> int --> bool)      ( =  )       vs
  | Pintcomp    Cneq   -> lift2 (int --> int --> bool)      ( <> )       vs
  | Pintcomp    Clt    -> lift2 (int --> int --> bool)      ( <  )       vs
  | Pintcomp    Cgt    -> lift2 (int --> int --> bool)      ( >  )       vs
  | Pintcomp    Cle    -> lift2 (int --> int --> bool)      ( <= )       vs
  | Pintcomp    Cge    -> lift2 (int --> int --> bool)      ( >= )       vs
  | Pfloatcomp  Ceq    -> lift2 (float --> float --> bool)  ( =  )       vs
  | Pfloatcomp  Cneq   -> lift2 (float --> float --> bool)  ( <> )       vs
  | Pfloatcomp  Clt    -> lift2 (float --> float --> bool)  ( <  )       vs
  | Pfloatcomp  Cgt    -> lift2 (float --> float --> bool)  ( >  )       vs
  | Pfloatcomp  Cle    -> lift2 (float --> float --> bool)  ( <= )       vs
  | Pfloatcomp  Cge    -> lift2 (float --> float --> bool)  ( >= )       vs
  | Pintoffloat        -> lift  (float --> int  )           int_of_float vs
  | Pfloatofint        -> lift  (int   --> float)           float_of_int vs
  | Pnegfloat          -> lift  (float --> float)           ( ~-. )      vs
  | Pabsfloat          -> lift  (float --> float)           abs_float    vs
  | Paddfloat          -> lift2 (float --> float --> float) ( +. )       vs
  | Psubfloat          -> lift2 (float --> float --> float) ( -. )       vs
  | Pmulfloat          -> lift2 (float --> float --> float) ( *. )       vs
  | Pdivfloat          -> lift2 (float --> float --> float) ( /. )       vs

  | Pstringlength -> lift  (str --> int)                   String.length     vs
  | Pstringrefu   -> lift2 (str --> int --> char)          String.unsafe_get vs
  | Pstringrefs   -> lift2 (str --> int --> char)          String.get        vs
  | Pstringsetu   -> lift3 (str --> int --> char --> unit) String.unsafe_set vs
  | Pstringsets   -> lift3 (str --> int --> char --> unit) String.set        vs

  | Pfield i ->
     lift (any --> any) (fun blk -> Obj.field blk i) vs

  | Psetfield (i, _) ->
     lift2 (any --> any --> unit) (fun blk v -> Obj.set_field blk i v) vs

  | Plazyforce -> lift (lazy' --> any) Lazy.force vs

  | Pmakearray _ -> Obj.repr @$ Array.of_list @$ List.map Obj.repr vs

  | Pmakeblock (tag, _) ->
     let blk = Obj.new_block tag @$ List.length vs in

     List.iteri (fun i v -> Obj.set_field blk i v) vs;
     blk

  | Parraylength _ -> lift  (array --> int) Array.length vs
  | Parrayrefu   _ -> lift2 (array --> int --> any)          Array.unsafe_get          vs
  | Parrayrefs   _ -> lift2 (array --> int --> any)          (fun a i -> a.(i))        vs
  | Parraysetu   _ -> lift3 (array --> int --> any --> unit) Array.unsafe_set          vs
  | Parraysets   _ -> lift3 (array --> int --> any --> unit) (fun a i e -> a.(i) <- e) vs

  | Pisint -> lift (any --> bool) Obj.is_int vs

  | Pbintofint Pint32     -> lift (int    --> int32 ) Int32.of_int     vs
  | Pbintofint Pint64     -> lift (int    --> int64 ) Int64.of_int     vs
  | Pbintofint Pnativeint -> lift (int    --> natint) Nativeint.of_int vs
  | Pintofbint Pint32     -> lift (int32  --> int)    Int32.to_int     vs
  | Pintofbint Pint64     -> lift (int64  --> int)    Int64.to_int     vs
  | Pintofbint Pnativeint -> lift (natint --> int)    Nativeint.to_int vs

  | Pcvtbint (Pint64, Pint32    ) -> lift (int64  --> int32 ) Int64.to_int32     vs
  | Pcvtbint (Pint32, Pint64    ) -> lift (int32  --> int64 ) Int64.of_int32     vs
  | Pcvtbint (Pnativeint, Pint32) -> lift (natint --> int32 ) Nativeint.to_int32 vs
  | Pcvtbint (Pnativeint, Pint64) -> lift (natint --> int64 ) Int64.of_nativeint vs
  | Pcvtbint (Pint32, Pnativeint) -> lift (int32  --> natint) Nativeint.of_int32 vs
  | Pcvtbint (Pint64, Pnativeint) -> lift (int64  --> natint) Int64.to_nativeint vs
  | Pcvtbint _                    -> assert false

  | Pnegbint Pint32     -> lift (int32  --> int32 ) Int32.neg     vs
  | Pnegbint Pint64     -> lift (int64  --> int64 ) Int64.neg     vs
  | Pnegbint Pnativeint -> lift (natint --> natint) Nativeint.neg vs

  | Paddbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.add     vs
  | Paddbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.add     vs
  | Paddbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.add vs

  | Psubbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.sub     vs
  | Psubbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.sub     vs
  | Psubbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.sub vs

  | Pmulbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.mul     vs
  | Pmulbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.mul     vs
  | Pmulbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.mul vs

  | Pdivbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.div     vs
  | Pdivbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.div     vs
  | Pdivbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.div vs

  | Pmodbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.rem     vs
  | Pmodbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.rem     vs
  | Pmodbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.rem vs

  | Pandbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.logand     vs
  | Pandbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.logand     vs
  | Pandbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.logand vs

  | Porbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.logor     vs
  | Porbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.logor     vs
  | Porbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.logor vs

  | Pxorbint Pint32     -> lift2 (int32  --> int32  --> int32 ) Int32.logxor     vs
  | Pxorbint Pint64     -> lift2 (int64  --> int64  --> int64 ) Int64.logxor     vs
  | Pxorbint Pnativeint -> lift2 (natint --> natint --> natint) Nativeint.logxor vs

  | Plslbint Pint32     -> lift2 (int32  --> int --> int32 ) Int32.shift_left     vs
  | Plslbint Pint64     -> lift2 (int64  --> int --> int64 ) Int64.shift_left     vs
  | Plslbint Pnativeint -> lift2 (natint --> int --> natint) Nativeint.shift_left vs

  | Plsrbint Pint32     -> lift2 (int32  --> int --> int32 ) Int32.shift_right_logical     vs
  | Plsrbint Pint64     -> lift2 (int64  --> int --> int64 ) Int64.shift_right_logical     vs
  | Plsrbint Pnativeint -> lift2 (natint --> int --> natint) Nativeint.shift_right_logical vs

  | Pasrbint Pint32     -> lift2 (int32  --> int --> int32 ) Int32.shift_right     vs
  | Pasrbint Pint64     -> lift2 (int64  --> int --> int64 ) Int64.shift_right     vs
  | Pasrbint Pnativeint -> lift2 (natint --> int --> natint) Nativeint.shift_right vs

  | Pnot -> lift (bool --> bool) not vs

  | Pbintcomp (kind, cmp) ->
     let cmp_op f x y = match cmp with
       | Ceq  -> f x y =  0
       | Cneq -> f x y <> 0
       | Clt  -> f x y <  0
       | Cgt  -> f x y >  0
       | Cle  -> f x y <= 0
       | Cge  -> f x y >= 0
     in
     let lift =
       match kind with
       | Pint32     -> lift2 (int32  --> int32  --> bool) (cmp_op Int32.compare    )
       | Pint64     -> lift2 (int64  --> int64  --> bool) (cmp_op Int64.compare    )
       | Pnativeint -> lift2 (natint --> natint --> bool) (cmp_op Nativeint.compare)
     in

     lift vs

  | Pstring_load_16 unsafe ->
     lift2 (str --> int --> char) (str_get_16 unsafe) vs

  | Pstring_load_32 unsafe ->
     lift2 (str --> int --> char) (str_get_32 unsafe) vs

  | Pstring_load_64 unsafe ->
     lift2 (str --> int --> char) (str_get_64 unsafe) vs

  | Pstring_set_16 unsafe ->
     lift3 (str --> int --> char --> unit) (str_set_16 unsafe) vs

  | Pstring_set_32 unsafe ->
     lift3 (str --> int --> char --> unit) (str_set_32 unsafe) vs

  | Pstring_set_64 unsafe ->
     lift3 (str --> int --> char --> unit) (str_set_64 unsafe) vs

  | Pbigstring_load_16 unsafe ->
     lift2 (bigstr --> int --> char) (bigstr_get_16 unsafe) vs

  | Pbigstring_load_32 unsafe ->
     lift2 (bigstr --> int --> char) (bigstr_get_32 unsafe) vs

  | Pbigstring_load_64 unsafe ->
     lift2 (bigstr --> int --> char) (bigstr_get_64 unsafe) vs

  | Pbigstring_set_16 unsafe ->
     lift3 (bigstr --> int --> char --> unit) (bigstr_set_16 unsafe) vs

  | Pbigstring_set_32 unsafe ->
     lift3 (bigstr --> int --> char --> unit) (bigstr_set_32 unsafe) vs

  | Pbigstring_set_64 unsafe ->
     lift3 (bigstr --> int --> char --> unit) (bigstr_set_64 unsafe) vs

  | Pctconst Big_endian    -> Obj.repr @$ Sys.big_endian
  | Pctconst Word_size     -> Obj.repr @$ Sys.word_size
  | Pctconst Ostype_unix   -> Obj.repr @$ Sys.unix
  | Pctconst Ostype_win32  -> Obj.repr @$ Sys.win32
  | Pctconst Ostype_cygwin -> Obj.repr @$ Sys.cygwin

  | Pccall call ->
     let name = call.Primitive.prim_name in

     Obj.repr @$
       begin
         match call.Primitive.prim_arity with
         | 0 -> assert false
         | 1 -> lift  (any --> any)                 (tamasheq_call_1 name) vs
         | 2 -> lift2 (any --> any --> any)         (tamasheq_call_2 name) vs
         | 3 -> lift3 (any --> any --> any --> any) (tamasheq_call_3 name) vs

         | 4 ->
            begin
              match vs with
              | [x1; x2; x3; x4] -> tamasheq_call_4 name x1 x2 x3 x4
              | xs               -> unsupported_arity 4 @$ List.length xs
            end

         | 5 ->
            begin
              match vs with
              | [x1; x2; x3; x4; x5] -> tamasheq_call_5 name x1 x2 x3 x4 x5
              | xs                   -> unsupported_arity 5 @$ List.length xs
            end

         | n ->
            failwith @$ sprintf "unhandled arity %d" n
       end

  | Pduprecord _  -> lift (any --> any) dup vs
  | Pgetglobal id -> lookup_global env id
  | Pidentity     -> lift (any --> any) (fun x -> x) vs
  | Pignore       -> nil
  | Pisout        -> lift2 (int --> int --> bool)  ( < )                  vs
  | Poffsetint i  -> lift  (int --> int )          (fun j -> i + j      ) vs
  | Poffsetref i  -> lift  (any --> unit)          (fun r -> r := !r + 1) vs
  | Praise        -> lift  (exn --> any)           raise                  vs
  | Psetglobal id -> lift  (any --> unit)          (bind_global env id)   vs

and eval_raise ks tag args =
  try  apply_fun (List.assoc tag ks) args
  with Not_found -> failwith "unknown continuation tag"

and eval_switch env ks body switch =
  let body = eval env ks body in

  try
    if Obj.is_block body then
      eval env ks @$ List.assoc (Obj.tag body) switch.sw_blocks
    else
      eval env ks @$ List.assoc (cast TyInt body) switch.sw_consts
  with exn ->
    match switch.sw_failaction with
    | None        -> raise exn
    | Some action -> eval env ks action

and eval_try_with env ks body id hdl =
  try  eval env ks body
  with exn -> eval (bind_lexical env id @$ Obj.repr exn) ks hdl

and eval_while env ks pred body =
  while cast TyBool @$ eval env ks pred do
    ignore @$ eval env ks body
  done;
  nil
