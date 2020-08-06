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

open Xml

let print_pcdata chan text =
  let l = String.length text in
  for p = 0 to l-1 do
    match text.[p] with
    | '>' -> output_string chan "&gt;"
    | '<' -> output_string chan "&lt;"
    | '&' ->
      if p < l-1 && text.[p+1] = '#' then
        output_char chan '&'
      else
        output_string chan "&amp;"
    | '\'' -> output_string chan "&apos;"
    | '"' -> output_string chan "&quot;"
    | c -> output_char chan c
  done

let print_attr chan (n,v) =
  output_char chan ' ';
  output_string chan n;
  output_string chan "=\"";
  let l = String.length v in
  for p = 0 to l-1 do
    match v.[p] with
    | '\\' -> output_string chan "\\\\"
    | '"' -> output_string chan "\\\""
    | c -> output_char chan c
  done;
  output_char chan '"'

let print chan x =
  let pcdata = ref false in
  let rec loop = function
    | Element (tag,alist,[]) ->
      output_char chan '<';
      output_string chan tag;
      List.iter (print_attr chan) alist;
      output_string chan "/>";
      pcdata := false;
    | Element (tag,alist,l) ->
      output_char chan '<';
      output_string chan tag;
      List.iter (print_attr chan) alist;
      output_char chan '>';
      pcdata := false;
      List.iter loop l;
      output_string chan "</";
      output_string chan tag;
      output_char chan '>';
      pcdata := false;
    | PCData text ->
      if !pcdata then output_char chan ' ';
      print_pcdata chan text;
      pcdata := true;
  in
  loop x

let print_fmt chan x =
  let rec loop ?(newl=false) tab = function
    | Element (tag,alist,[])  ->
      output_string chan tab;
      output_char chan '<';
      output_string chan tag;
      List.iter (print_attr chan) alist;
      output_string chan "/>";
      if newl then output_char chan '\n';
    | Element (tag,alist,[PCData text])  ->
      output_string chan tab;
      output_char chan '<';
      output_string chan tag;
      List.iter (print_attr chan) alist;
      output_string chan ">";
      print_pcdata chan text;
      output_string chan "</";
      output_string chan tag;
      output_char chan '>';
      if newl then output_char chan '\n';
    | Element (tag,alist,l)  ->
      output_string chan tab;
      output_char chan '<';
      output_string chan tag;
      List.iter (print_attr chan) alist;
      output_string chan ">\n";
      List.iter (loop ~newl:true (tab^"  ")) l;
      output_string chan tab;
      output_string chan "</";
      output_string chan tag;
      output_char chan '>';
      if newl then output_char chan '\n';
    | PCData text ->
      print_pcdata chan text;
      if newl then output_char chan '\n';
  in
  loop "" x;
