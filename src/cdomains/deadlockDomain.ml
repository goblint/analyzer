(** Deadlock domain. *)

module Lock = LockDomain.Addr
module LockEvent = Printable.Prod3 (Lock) (Node) (MCPAccess.A)

module MayLockEvents = SetDomain.ToppedSet (LockEvent) (struct let topname = "All lock events" end)
