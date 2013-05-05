w1 "file handle is not saved!"
w2 "closeing unopened file handle $"
w3 "writing to unopened file handle $"
w4 "writing to read-only file handle $"
w5 "closeing already closed file handle $"
w6 "writing to closed file handle $"
w7 "overwriting still opened file handle $"

1          -> w1            fopen(_)
1          -> w2            fclose($fp)
1          -> w3            fprintf($fp, _)
1          -> open_read     $fp = fopen($path, "r")
1          -> open_write    $fp = fopen($path, "w")
1          -> open_write    $fp = fopen($path, "a")
open_read  -> w4            fprintf($fp, _)
// open_write -> open_write    fprintf($fp, _) // not needed, but changes loc

// open_read  -> w7, $1     $fp = fopen($path, _)
// open_wite  -> w7, $1     $fp = fopen($path, _)

open_read  -> closed        fclose($fp)
open_write -> closed        fclose($fp)
closed     -> w5            fclose($fp)
closed     -> w6            fprintf($fp, _)
// TODO forwarding, for now do explicit transitions instead
// closed     -> 1             $_
closed     -> w1            fopen(_)
closed     -> open_read     $fp = fopen($path, "r")
closed     -> open_write    $fp = fopen($path, "w")
closed     -> open_write    $fp = fopen($path, "a")

// setup which states are end states
1          -> end           _
closed     -> end           _
// warning for all entries that are not in an end states
_end "file is never closed"