open Batteries

module D = LvalMapDomain


module Val =
struct
  type mode = Read | Write
  type s = Open of string*mode | Closed | Error
  let name = "File handles"
  let var_state = Closed
  let string_of_mode = function Read -> "Read" | Write -> "Write"
  let string_of_state = function
    | Open(filename, m) -> "open("^filename^", "^string_of_mode m^")"
    | Closed -> "closed"
    | Error  -> "error"

  (* properties of records (e.g. used by Dom.report) *)
  let opened   s = s <> Closed && s <> Error
  let closed   s = s = Closed
  let writable s = match s with Open((_,Write)) -> true | _ -> false
end


module Dom =
struct
  include D.Domain (D.Value (Val))

  (* returns a tuple (thunk, result) *)
  let report_ ?neg:(neg=false) k p msg m =
    let f ?may:(may=false) msg =
      let f () = warn ~may:may msg in
      f, if may then `May true else `Must true in
    let mf = (fun () -> ()), `Must false in
    if mem k m then
      let p = if neg then not % p else p in
      let v = find' k m in
      if V.must p v then f msg (* must *)
      else if V.may p v then f ~may:true msg (* may *)
      else mf (* none *)
    else if neg then f msg else mf

  let report ?neg:(neg=false) k p msg m = (fst (report_ ~neg:neg k p msg m)) () (* evaluate thunk *)

  let reports k xs m =
    let uncurry (neg, p, msg) = report_ ~neg:neg k p msg m in
    let f result x = if snd (uncurry x) = result then Some (fst (uncurry x)) else None in
    let must_true = BatList.filter_map (f (`Must true)) xs in
    let may_true  = BatList.filter_map (f (`May true)) xs in
    (* output first must and first may *)
    if List.length must_true > 0 then (List.hd must_true) ();
    if List.length may_true  > 0 then (List.hd may_true) ()

  (* handling state *)
  let opened   r = V.state r |> Val.opened
  let closed   r = V.state r |> Val.closed
  let writable r = V.state r |> Val.writable

  let fopen k loc filename mode m =
    if is_unknown k m then m else
    let mode = match String.lowercase mode with "r" -> Val.Read | _ -> Val.Write in
    let v = V.make k loc (Val.Open(filename, mode)) in
    add' k v m
  let fclose k loc m =
    if is_unknown k m then m else
    let v = V.make k loc Val.Closed in
    change k v m
  let error k m =
    if is_unknown k m then m else
    let loc = if mem k m then find' k m |> V.split |> snd |> Set.choose |> V.loc else [] in
    let v = V.make k loc Val.Error in
    change k v m
  let success k m =
    if is_unknown k m then m else
    let v = find' k m in
    if V.may (Val.opened%V.state) v && V.may (V.in_state Val.Error) v then
      change k (V.filter (Val.opened%V.state) v) m
    else
      m
end
