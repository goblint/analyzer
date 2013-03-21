w1 "file handle is not saved!"
w2 "closeing unopened file handle"
w3 "writing to unopened file handle"
w4 "writing to read-only file handle"
w5 "closeing already closed file handle"
w6 "writing to closed file handle"

1          -> w1            fopen(_)
1          -> w2            fclose($fp)
1          -> w3            fprintf($fp, _)
1          -> open_read     $fp = fopen($path, "r")
1          -> open_write    $fp = fopen($path, "w")
1          -> open_write    $fp = fopen($path, "a")
open_read  -> w4            fprintf($fp, _)
open_write -> open_write    fprintf($fp, _)
open_read  -> closed        fclose($fp)
open_write -> closed        fclose($fp)
closed     -> w5            fclose($fp)
closed     -> w6            fprintf($fp, _)
closed     -> 1             $_
