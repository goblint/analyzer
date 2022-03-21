open Cil
open Pretty

module Lock = LockDomain.Addr
module LockEvent = Printable.Prod (Lock) (Node)

module MayLockEvents = SetDomain.ToppedSet (LockEvent) (struct let topname = "All lock events" end)
