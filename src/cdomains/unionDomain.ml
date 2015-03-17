module Field =  Lattice.Flat (Basetype.CilField) (struct
    let top_name = "Unknown field"
    let bot_name = "If you see this, you are special!"
  end)

module Simple (Values: Lattice.S) = Lattice.Prod (Field) (Values)
