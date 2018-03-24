open QCheck

let shrink arb = BatOption.default Shrink.nil arb.shrink