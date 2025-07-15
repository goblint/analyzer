(* Simple Mutex Implementation using Domain-Local Await (https://github.com/ocaml-multicore/domain-local-await)
   Copyright © 2023 Vesa Karvonen

   Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
   provided that the above copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
   OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
   ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

type 'a state =
  | Fun of (unit -> 'a)
  | Run of (unit -> unit) list
  | Val of 'a
  | Exn of exn

type 'a t = 'a state Atomic.t

let from_fun th = Atomic.make (Fun th)
(* val from_fun : (unit -> 'a) -> 'a state Atomic.t = <fun> *)

let from_val v = Atomic.make (Val v)
(* val from_val : 'a -> 'a state Atomic.t = <fun> *)

let rec force t =
  match Atomic.get t with
  | Val v -> v
  | Exn e -> raise e
  | Fun th as before ->
    if Atomic.compare_and_set t before (Run []) then
      let result =
        match th () with
        | v -> Val v
        | exception e -> Exn e
      in
      match Atomic.exchange t result with
      | (Val _ | Exn _ | Fun _) ->
        failwith "impossible"
      | Run waiters ->
        List.iter ((|>) ()) waiters;
        force t
    else
      force t
  | Run waiters as before ->
    let dla = Domain_local_await.prepare_for_await () in
    let after = Run (dla.release :: waiters) in
    if Atomic.compare_and_set t before after then
      match dla.await () with
      | () ->
        force t
      | exception cancelation_exn ->
        let rec cleanup () =
          match Atomic.get t with
          | (Val _ | Exn _ | Fun _) ->
            ()
          | Run waiters as before ->
            let after = Run (List.filter ((!=) dla.release) waiters) in
            if not (Atomic.compare_and_set t before after) then
              cleanup ()
        in
        cleanup ();
        raise cancelation_exn
    else
      force t
