w1 "file handle is not saved!"
w2 "closeing unopened file handle $"
w3 "writing to unopened file handle $"
w4 "writing to read-only file handle $"
w5 "closeing already closed file handle $"
w6 "writing to closed file handle $"
w7 "overwriting still opened file handle $"
w8 "unrecognized file open mode for file handle $"

// TODO later add fputs and stuff
1          -> w1            fopen(_)
1          -> w2            fclose($fp)
1          -> w3            fprintf($fp, _)
//1          -> open_read     $fp = fopen($path, "r")
//1          -> open_write    $fp = fopen($path, r"[wa]") // see OCaml doc for details (e.g. \\| for alternatives)
//1          -> w8            $fp = fopen($path, _)

// go to unchecked states first
1          -> u_open_read  $fp = fopen($path, "r")
1          -> u_open_write $fp = fopen($path, r"[wa]")
1          -> w8            $fp = fopen($path, _)
// once branch(exp, tv) is matched, return dom with 1. arg (lval = exp) and true/false
// forwarding from branch is not possible (would need an extra map for storing states) -> ignore it
// warnings are also ignored
// then in branch take out lval, check exp and do the transition to a checked state
u_open_read  -> 1           branch($key==0, true)
u_open_read  -> open_read   branch($key==0, false)
u_open_write -> 1           branch($key==0, true)
u_open_write -> open_write  branch($key==0, false)

// alternative: forward everything. Problem: saving arguments of call (special_fn -> branch -> special_fn)
// 1          ->> open_check   $fp = fopen($path, _)
// open_check ->> 1            branch($fp==0, true)
// open_check ->> open         branch($fp==0, false)
// open       -> open_read     $fp = fopen($path, "r")
// open       -> open_write    $fp = fopen($path, "[wa]")
// open       -> w8            $fp = fopen($path, _)

open_read  -> w4            fprintf($fp, _)
// open_write -> open_write    fprintf($fp, _) // not needed, but changes loc

open_read  -w7>> 1          $fp = fopen($path, _)
open_write -w7>> 1          $fp = fopen($path, _)

open_read  -> closed        fclose($fp)
open_write -> closed        fclose($fp)
closed     -> w5            fclose($fp)
closed     -> w6            fprintf($fp, _)
closed     ->> 1            _ // let state 1 handle the rest

// setup which states are end states
1          -> end           _
closed     -> end           _
// warning for all entries that are not in an end states
_end "file is never closed"
_END "unclosed files: $"