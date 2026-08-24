(* Simple Mutex Implementation using Domain-Local Await (https://github.com/ocaml-multicore/domain-local-await)
   Copyright © 2023 Vesa Karvonen

   Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
   provided that the above copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
   OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
   ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

type state =
  | Unlocked
  | Locked of (unit -> unit) list

type t = state Atomic.t

let create () = Atomic.make Unlocked

let unlock t =
  match Atomic.exchange t Unlocked with
  | Unlocked -> invalid_arg "mutex: already unlocked"
  | Locked awaiters -> List.iter ((|>) ()) awaiters

let rec lock t =
  match Atomic.get t with
  | Unlocked ->
    if not (Atomic.compare_and_set t Unlocked (Locked [])) then
      lock t
  | Locked awaiters as before ->
    let dla = Domain_local_await.prepare_for_await () in
    let after = Locked (dla.release :: awaiters) in
    if Atomic.compare_and_set t before after then
      match dla.await () with
      | () -> lock t
      | exception cancellation_exn ->
        let rec cleanup () =
          match Atomic.get t with
          | Unlocked -> ()
          | Locked awaiters as before ->
            if List.for_all ((==) dla.release) awaiters then
              let after =
                Locked (List.filter ((!=) dla.release) awaiters)
              in
              if not (Atomic.compare_and_set t before after) then
                cleanup ()
        in
        cleanup ();
        raise cancellation_exn
    else
      lock t
