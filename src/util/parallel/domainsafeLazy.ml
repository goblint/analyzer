(* Based on the lazy implementation using Domain-Local Await (https://github.com/ocaml-multicore/domain-local-await)
   Copyright © 2023 Vesa Karvonen

   Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
   provided that the above copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
   OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
   ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(* While another domain is forcing, waiters spin instead of awaiting via Domain-Local Await:
   these lazies are forced in transfer functions, and under domainslib awaiting suspends the task and runs other tasks
   on the same domain (or resumes it on another), which clobbers domain-local state of the suspended transfer function
   (e.g. current node, speculative mode). The thunks are short and only run once, so spinning is cheap. *)

type 'a state =
  | Fun of (unit -> 'a)
  | Run
  | Val of 'a
  | Exn of exn

type 'a t = 'a state Atomic.t

let from_fun th = Atomic.make (Fun th)

let from_val v = Atomic.make (Val v)

let rec force t =
  match Atomic.get t with
  | Val v -> v
  | Exn e -> raise e
  | Fun th as before ->
    if Atomic.compare_and_set t before Run then (
      let result =
        match th () with
        | v -> Val v
        | exception e -> Exn e
      in
      Atomic.set t result;
      force t
    )
    else
      force t
  | Run ->
    Domain.cpu_relax ();
    force t
