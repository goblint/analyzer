open Printf

let loose_dtd c =
  fprintf c "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n"

let rec alist c xs =
  match xs with
  | []    -> ()
  | (x,xv)::xs ->
    fprintf c " %s=\"%s\"" x xv;
    alist c xs

let tag tn ?(tp=[]) tc c =
  fprintf c "<%s%a>" tn alist tp;
  tc c;
  fprintf c "</%s>\n" tn


let str s c = fprintf c "%s" s
let num n c = fprintf c "%d" n

let (<:>) f g c = f c; g c

let newfile c ct =
  loose_dtd c;
  tag "html" ct c

let rec ehcol tn ?(rp=[]) xs =
  match xs,rp with
  | [],_        -> fun _ -> ()
  | x::xs,[]    -> tag tn x       <:> ehcol tn xs
  | x::xs,r::rp -> tag tn ~tp:r x <:> ehcol tn ~rp:rp xs

let hcol ?(rp=[]) xs = ehcol "th" ~rp:rp xs
let col  ?(rp=[]) xs = ehcol "td" ~rp:rp xs

let rec row ?(rp=[]) = function
  | []     -> fun _ -> ()
  | xs::xss -> tag "tr" (col ~rp:rp xs) <:> row xss

let hrow ?(rp=[]) = function
  | []     -> fun _ -> ()
  | xs::xss -> tag "tr" (hcol ~rp:rp xs) <:> row xss

let table ?(tp=[]) ?(rp=[]) t =
  tag "table" ~tp:tp (hrow ~rp:rp t)

let table' ?(tp=[]) ?(rp=[]) t =
  tag "table" ~tp:tp (row ~rp:rp t)

let tablehead ?(tp=[]) th t =
  tag "table" ~tp:tp (th <:> row t)
