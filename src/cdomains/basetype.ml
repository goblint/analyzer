(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

module GU = Goblintutil
open Cil
open Pretty

module ProgLines : Printable.S with type t = location =
struct
  type t = location
  let isSimple _  = true
  let copy x = x
  let equal x y = 
    x.line = y.line && x.file = y.file
  let compare x y = compare (x.file, x.line) (y.file, y.line)
  let hash x = Hashtbl.hash (x.line, x.file)
  let toXML_f _ x = Xml.Element ("Loc", [("file", x.file); ("line", string_of_int x.line)], [])
  let short _ x = x.file ^ ":" ^ string_of_int x.line
  let pretty_f sf () x = text (sf max_int x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "proglines"
end

module ProgLinesFun: Printable.S with type t = location * fundec =
struct
  type t = location * fundec
  let isSimple _  = true
  let copy x = x
  let equal (x,_) (y,_) = ProgLines.equal x y
  let compare (x,_) (y,_) = ProgLines.compare x y
  let hash (x,_) = ProgLines.hash x
  let toXML_f _ (x,f) = Xml.Element ("Loc", [("file", x.file); 
					     ("line", string_of_int x.line); 
					     ("fun", f.svar.vname)], [])
  let short w (x,f) = ProgLines.short w x ^ "(" ^ f.svar.vname ^ ")"
  let pretty_f sf () x = text (sf max_int x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "proglinesfun"
end

module Variables = 
struct
  type t = varinfo
  let isSimple _  = true
  let copy x = x
  let equal x y = x.vid = y.vid
  let compare x y = compare x.vid y.vid
  let hash x = Hashtbl.hash x.vid
  let short _ x = x.vname
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
    let typeinf = Pretty.sprint Goblintutil.summary_length (d_type () x.vtype) in
    let info = "id=" ^ string_of_int x.vid ^ "; type=" ^ esc typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int x)); ("info", info)],[])
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () x = Pretty.dprintf "%s on %a" x.vname ProgLines.pretty x.vdecl
  let get_location x = x.vdecl
  let classify x = match x with
    | x when x.vglob -> 2
    | x when x.vdecl.line = -1 -> -1
    | x when x.vdecl.line = -3 -> 5
    | x when x.vdecl.line = -4 -> 4
    | _ -> 1
  let class_name n = match n with
    |  1 -> "Local"
    |  2 -> "Global"
    |  4 -> "Context"
    |  5 -> "Parameter"
    | -1 -> "Temp"
    |  _ -> "None"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "variables"
end

module VarStatus =
struct
  type status = Local | Context
  type t = varinfo * status
  let isSimple _  = true
  let copy x = x
  let equal (x,sx) (y,sy) = x.vid = y.vid && sx = sy
  let compare (x,sx) (y,sy) = compare (x.vid,sx) (y.vid,sy)
  let hash (x,s) = Hashtbl.hash (x.vid,s)
  let short _ (x,s) = x.vname
  let toXML_f sf (x,_ as xs) = 
    let esc = Goblintutil.escape in
    let typeinf = Pretty.sprint Goblintutil.summary_length (d_type () x.vtype) in
    let info = "id=" ^ string_of_int x.vid ^ "; type=" ^ esc typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int xs)); ("info", info)],[])
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () (x,s) = Pretty.dprintf "%s on %a" x.vname ProgLines.pretty x.vdecl
  let get_location (x,s) = x.vdecl
  let classify x = match x with
    | x,_ when x.vglob -> 2
    | x,_ when x.vdecl.line = -1 -> -1
    | x,_ when x.vdecl.line = -3 -> 5
    | _, Context -> 4
    | _, _ -> 1
  let class_name n = match n with
    |  1 -> "Local"
    |  2 -> "Global"
    |  4 -> "Context"
    |  5 -> "Parameter"
    | -1 -> "Temp"
    |  _ -> "None"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "variables"
end

module RawStrings: Printable.S with type t = string = 
struct
  include Printable.Std
  open Pretty
  type t = string
  let isSimple _ = true
  let short _ x = "\"" ^ x ^ "\""
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
      Xml.Element ("Leaf", ["text", esc (sf 80 x)], [])
  let pretty_f sf () x = text (sf 80 x) 
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "raw strings"
end

module Strings: Lattice.S with type t = [`Bot | `Lifted of string | `Top] =
  Lattice.Flat (RawStrings) (struct 
                               let top_name = "?"
                               let bot_name = "-"
                             end)

module CilStmt: Printable.S with type t = stmt =
struct
  type t = stmt
  let isSimple _  = false
  let copy x = x
  let compare x y = compare x.sid y.sid
  let equal x y = x.sid = y.sid
  let hash x = Hashtbl.hash (x.sid)
  let short _ x = "<stmt>"
  let toXML_f _ x = Xml.Element ("Stmt", [("id", string_of_int x.sid); 
					  ("sourcecode", Pretty.sprint ~width:0 (d_stmt () x))], [])
  let pretty_f _ () x = 
    match x.skind with
      | Instr (y::ys) -> d_instr () y
      | If (exp,_,_,_) -> d_exp () exp
      | _ -> d_stmt () x

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "strings"
end

module CilFun: Printable.S with type t = varinfo =
struct
  let isSimple _  = false
  let copy x = x
  type t = varinfo
  let compare x y = compare x.vid y.vid
  let equal x y = x.vid = y.vid
  let hash x = Hashtbl.hash x.vid
  let toXML_f _ x = Xml.Element ("Fun", [("id", string_of_int x.vid); 
					 ("name", x.vname)], [])
  let short _ x = x.vname
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "functions"
end

module CilFundec =
struct
  let isSimple _  = false
  let copy x = x
  type t = fundec
  let compare x y = compare x.svar.vid y.svar.vid
  let equal x y = x.svar.vid = y.svar.vid
  let hash x = Hashtbl.hash x.svar.vid
  let toXML_f _ x = CilFun.toXML x.svar
  let short _ x = x.svar.vname
  let pretty_f _ () x = CilFun.pretty () x.svar
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "function decs"
  let dummy = dummyFunDec
end

module CilField: MapDomain.Groupable with type t = fieldinfo =
struct
  let isSimple _  = true
  let copy x = x
  type t = fieldinfo
  let compare x y = compare x.fname y.fname
  let equal x y = x.fname = y.fname
  let hash x = Hashtbl.hash x.fname
  let short _ x = x.fname
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
      Xml.Element ("Leaf", [("text", esc (sf max_int x))], [])
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let classify _ = 0
  let class_name _ = "None"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "field"
end

