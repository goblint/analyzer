let time name f x =
  let start = Catapult.Tracing.begin_ () in
  Fun.protect (fun () ->
      Stats.time name f x
    ) ~finally:(fun () ->
      Catapult.Tracing.exit name start
    )
