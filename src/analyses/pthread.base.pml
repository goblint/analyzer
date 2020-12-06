

// helpers for scheduling etc.
#define oneIs(v) checkStatus(==, v, ||)
#define oneIsNot(v) checkStatus(!=, v, ||)
#define allAre(v) checkStatus(==, v, &&)
#define noneAre(v) checkStatus(!=, v, &&)

// status: NOTCREATED, STOPPED, SUSPENDED, WAITING, READY, (RUNNING), DONE
// LTL formulas
#define notStarving(i) (always (status[i] == READY implies always eventually (status[i] == READY || status[i] == STOPPED || status[i] == DONE)))
ltl not_starving  { allTasks(notStarving) }
// ltl not_starving  { ! (eventually always oneIs(WAITING) || oneIs(SUSPENDED)) }
ltl not_waiting   { ! (eventually always oneIs(WAITING)) }
ltl not_suspended { ! (eventually always oneIs(SUSPENDED)) }
ltl all_created   { eventually always noneAre(NOTCREATED) }
// ltl created_then_ready { allAre(NOTCREATED U READY) }
// ltl all_ready { allAre(eventually READY) }
// periodic processes should never be done
// TODO: ltl periodic      { always periodic_noneAre(DONE) }
// TODO: ltl nonperiodic   { eventually nonperiodic_allAre(DONE) }
// starvation: process will always be READY but never RUNNING
// ltl pr { ! (eventually always oneIs(READY)) }
