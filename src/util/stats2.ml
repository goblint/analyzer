let landmarks = BatHashtbl.create 10

let time name f x =
  let landmark = match BatHashtbl.find_option landmarks name with
    | Some landmark -> landmark
    | None ->
      let landmark = Landmark.register name in
      BatHashtbl.replace landmarks name landmark;
      landmark
  in
  Landmark.enter landmark;
  let start = Catapult.Tracing.begin_ () in
  Fun.protect (fun () ->
      Stats.time name f x
    ) ~finally:(fun () ->
      Catapult.Tracing.exit name start;
      Landmark.exit landmark;
    )
