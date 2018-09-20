(** Formatstring analysis *)

open Prelude.Ana
open Analyses
open Str

module Q = Queries
module I64 = Int64
module MP = Messages.MkPool (struct let name = "Formatstrings" end)

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "formatstring"
  module D = Lattice.Unit
  module G = Lattice.Unit
  module C = Lattice.Unit

  (* format string parsing *)
  type format_part = 
    PrintSpecifier of string option * string option * string option * string option * char
    | ScanSpecifier of string option * string option * string option * char
    | String of string

  type format_string = format_part list 

  let string_of_split_result = function
    | Text s -> s
    | Delim s -> s

  let string_of_format_part fp = 
    let (~|) ?prefix:(prefix="") = function
      | Some s -> prefix^s
      | None -> ""
    in
    match fp with
    | PrintSpecifier (fl, wi, pr, le, ts) ->
      "%"^(~| fl)^(~| wi)^((~|) ~prefix:"." pr)^(~| le)^(String.make 1 ts)^" (print)"
    | ScanSpecifier (ig, wi, le, ts) -> 
      "%"^(~| ig)^(~| wi)^(~| le)^(String.make 1 ts)^" (scan)"
    | String s ->
      s^" (string)"

  let (~~) s = try int_of_string s with _ -> 0

  let cil_ikinds = [Cil.IChar; Cil.ISChar; Cil.IUChar; Cil.IBool; Cil.IInt; Cil.IUInt; Cil.IShort; Cil.IUShort; Cil.ILong; Cil.IULong; Cil.ILongLong; Cil.IULongLong]

  let print_rgx_str = "%\\([-\\+ #0]*\\)?\\(\\([0-9]\\)*\\|\\*\\)?\\(\\.\\(\\([0-9]\\)*\\|\\*\\)\\)?\\(hh\\|h\\|l\\|ll\\|j\\|z\\|t\\|L\\)?\\([diouxXfFeEgGaAcspn]\\)"

  let scan_rgx_str = "\\(%\\(\\*\\)?\\(\\([0-9]\\)*\\)?\\(hh\\|h\\|l\\|ll\\|j\\|z\\|t\\|L\\)?\\(\\(\\([iduoxfegacspn]\\)\\)\\|\\(\\[\\([\\^]?\\([^]]\\)+\\)\\]\\)\\)\\)"

  let print_rgx = Str.regexp print_rgx_str 
  let scan_rgx = Str.regexp scan_rgx_str 
  let format_rgx = Str.regexp (print_rgx_str ^ "\\|" ^ scan_rgx_str)

  let parse_format_str fs scan =
    let get_match_group i s =
      try
        let m = Str.matched_group i s in
        if String.length m = 0 then
          None
        else 
          Some m
      with _ -> None
    in
    let parse_delim = function
      | Text s -> String s
      | Delim fs ->
        if scan then ( 
          (* Setting up matched groups *)
          if Str.string_match scan_rgx fs 0 then
            let ts = 
            (match get_match_group 6 fs with
              | Some s ->
                  ( 
                  if String.length s = 1 then
                    String.get s 0
                  else
                    's' (* This is a [...] or [^...] which is equiv. to s *)
                  )
              | None -> failwith "Typespecifier failed" (* Cannot happen *)
            ) in
            let ig = get_match_group 2 fs in
            let wi = get_match_group 3 fs in
            let le = get_match_group 5 fs in
            ScanSpecifier (ig, wi, le, ts)
          else
            String fs
        ) else (
          (* Setting up matched groups *)
          if Str.string_match print_rgx fs 0 then 
            let ts = String.get fs ((String.length fs) - 1) in
            let le = get_match_group 7 fs in
            let pr = get_match_group 5 fs in
            let wi = get_match_group 2 fs in
            let fl = get_match_group 1 fs in
            PrintSpecifier (fl, wi, pr, le, ts)
          else
            String fs
        )
    in
    (* Workaround for escaped format specifers *)
    let fs = Str.global_replace (Str.regexp "%%") "_" fs in
    Str.full_split format_rgx fs 
    |> List.map 
        (fun x -> let res = parse_delim x in
          MP.debug ((string_of_split_result x)^" --> "^(string_of_format_part res));
          M.debug ((string_of_split_result x)^" --> "^(string_of_format_part res));
          res)

  let is_undelimited_string = function
    | PrintSpecifier (_, _, None, _, 's') -> true
    | ScanSpecifier (None, None, _, 's') -> true
    | _ -> false

  let has_undelimited_string = List.exists is_undelimited_string

  let filter_nonformatting =
    List.filter (
      fun x -> match x with
        | String _ -> false
        | ScanSpecifier (Some _,_,_,_) -> false
        | _ -> true
    )

  (*
   * TODO: Approx. size
   *
  type format_size = Fixed of int | Interval of int * int | Unlimited

  let (+|) a b = match a, b with
    | Fixed x, Fixed y -> Fixed (x+y)
    | Interval (x1,x2), Interval (y1,y2) -> Interval (x1+y1,x2+y2)
    | Fixed x, Interval (y,z) | Interval (y,z), Fixed x -> Interval (y+x,z+x)
    | Unlimited, _ | _, Unlimited -> Unlimited

  let format_spec_size = function
    | String str -> Fixed (String.length str)
    | PrintSpecifier (f,w,p,s,t) -> 
        begin
          match t, w, p with
            | 'n',_,_ -> Fixed 0
            | 's',Some wi,Some pr -> Interval (~~ wi, ~~ pr)
            | 's',None,Some pr -> Interval (0, ~~ pr)
            | 's',_,None -> Unlimited
            (* 
             * TODO: Get the maxmimum and minimum sizes for different
             * types and the size modifiers.
             *
             * TODO: Dfferentiate between type specifiers
             * Width and precision fields work completely differently
             * depending on the type specifier.
             *
             * TODO: Handle * within width and precision
             *
             * TODO: Handle flags
             *)
            | _,_,_ -> Fixed 1
        end

  let format_string_size =
    List.fold_left (fun a x -> a +| (format_spec_size x)) (Fixed 0)
  *)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  (* format function analysis *)
  type format_fun = Print of int | Scan of int | Other

  type type_comp = Specific of Cil.typ | Multiple of Cil.typ list 

  let format_fun_type = function
    | "printf" -> Print 0
    | "fprintf" -> Print 1
    | "dprintf" -> Print 1
    | "sprintf" -> Print 1
    | "snprintf" -> Print 2
    | "scanf" -> Scan 0
    | "fscanf" -> Scan 1
    | "sscanf" -> Scan 1
    | _ -> Other

  let format_spec_type fsp =
    let is_scan_spec = function
      | ScanSpecifier _ -> true
      | _ -> false
    in
    let ptr_of x = TPtr (x, [])
    in
    let report_lenmod l t = 
      MP.report ~reference:(CERT "FIO47-C") "Mod" (l^" is an undefined length modifier for "^(String.make 1 t))
    in
    match fsp with
      | String _ -> failwith "Need format spec"
      | ScanSpecifier (Some _,_,_,_) -> failwith "No type for ignored specifier"
      | PrintSpecifier (_,_,_,s,t) | ScanSpecifier (None,_,s,t) -> match t with
          | 'd' | 'i' | 'n' ->
              begin
                let ret = (match s with 
                  | Some "hh" -> TInt (ISChar, [])
                  | Some "h"  -> TInt (IShort, [])
                  | Some "l"  -> TInt (ILong, [])
                  | Some "ll" -> TInt (ILongLong, [])
                  | Some "j"  -> TInt (ILongLong, []) (* Assume that intmax_t = long long*)
                  | Some "z"  -> TInt (!Cil.kindOfSizeOf, [])
                  | Some "t"  -> !Cil.ptrdiffType
                  | Some x    -> (report_lenmod x t; TInt (IInt, []))
                  | None      -> TInt (IInt, [])
                ) in if ((t = 'n') || (is_scan_spec fsp)) then ptr_of ret else ret
              end
          | 'u' | 'o' | 'x' | 'X' ->
              begin
                let ret = (match s with 
                  | Some "hh" -> TInt (IUChar, [])
                  | Some "h"  -> TInt (IUShort, [])
                  | Some "l"  -> TInt (IULong, [])
                  | Some "ll" -> TInt (IULongLong, [])
                  | Some "j"  -> TInt (IULongLong, []) (* Assume that uintmax_t = uns. long long *)
                  | Some "z"  -> TInt (!Cil.kindOfSizeOf, [])
                  | Some "t"  -> !Cil.ptrdiffType
                  | Some x    -> (report_lenmod x t; TInt (IUInt, []))
                  | None      -> TInt (IUInt, [])
                ) in if is_scan_spec fsp then ptr_of ret else ret
              end
          | 'f' | 'F' | 'e' | 'E' | 'g' | 'G' | 'a' | 'A' ->
              begin
                match s with 
                  | Some "L" when is_scan_spec fsp -> TPtr (TFloat (FLongDouble, []), [])
                  | Some "L" -> TFloat (FLongDouble, [])
                  | Some "l" when is_scan_spec fsp -> TPtr (TFloat (FDouble, []), [])
                  | Some x when is_scan_spec fsp -> (report_lenmod x t; TPtr (TFloat (FFloat, []), []))
                  | Some x -> (report_lenmod x t; TPtr (TFloat (FFloat, []), []))
                  | None when is_scan_spec fsp -> TPtr (TFloat (FFloat, []), [])
                  | None -> TFloat (FDouble, [])
              end
          | 's' -> 
              begin
                match s with
                  | Some "l" -> TPtr (TInt (!Cil.wcharKind, []), [])
                  | Some x -> (report_lenmod x t; TPtr (TInt (IChar, []), []))
                  | None -> TPtr (TInt (IChar, []), [])
              end
          | 'c' when is_scan_spec fsp -> 
              begin
                match s with
                  | Some "l" -> TPtr (TInt (!Cil.wcharKind, []), [])
                  | Some x -> (report_lenmod x t; TPtr (TInt (IChar, []), []))
                  | None -> TPtr (TInt (IChar, []), [])
              end
          | 'c' -> 
              begin
                match s with
                  | Some "l" -> TInt (!Cil.wcharKind, [])
                  | Some x -> (report_lenmod x t; TPtr (TInt (IInt, []), []))
                  | None -> TPtr (TInt (IChar, []), [])
              end
          | 'p' -> 
              begin
                let ret = (match s with
                  | Some x -> (report_lenmod x t; TPtr (TVoid [], []))
                  | None -> TPtr (TVoid [], [])
                ) in if is_scan_spec fsp then ptr_of ret else ret
              end
          | _ -> failwith ("Impossible type for "^(string_of_format_part fsp))

  let string_of_type t = String.trim (Pretty.sprint 80 (Cil.d_type () t))

  let string_of_comp_type = function
    | Specific t -> string_of_type t
    | Multiple tx -> let ret = "possibly "^(
        List.map string_of_type tx 
        |> List.fold_left (fun acc s -> s^", "^acc) ""
      ) in String.sub ret 0 ((String.length ret)-2)

  let string_of_exp t = String.trim (Pretty.sprint 80 (Cil.d_exp () t))

  let format_args_types fs = 
    let aux fsp = match fsp with
      | PrintSpecifier (_,Some "*",Some "*",_,_) -> 
          [TInt (IInt, []); TInt (IInt, []); format_spec_type fsp]
      | PrintSpecifier (_,Some "*",_,_,_) | PrintSpecifier (_,_,Some "*",_,_) -> 
          [TInt (IInt, []); format_spec_type fsp]
      | PrintSpecifier (_,_,_,_,_) -> 
          [format_spec_type fsp]
      | ScanSpecifier (None,_,_,_) ->
          [format_spec_type fsp]
      | _ -> []
    in List.map aux fs |> List.flatten

  let flag_check fs =
    let warn flag typespec =
      let flag = String.make 1 flag in
      let typs = String.make 1 typespec in
      MP.report ~reference:(CERT "FIO47-C") "TypeSpec" ("The flag "^flag^" is undefined for the type-specifier "^typs)
    in
    let aux = function
      | PrintSpecifier(Some f,_,_,_,t) ->
          if String.contains "cspn" t && String.contains f '0' then
            warn '0' t
          else if String.contains "cspndiu" t && String.contains f '#' then
            warn '#' t
          else
            ()
      | _ -> ()
    in List.iter aux fs

  let type_check fs arglist =
    let iteri2 f al bl =
      let rec aux f i al bl = match al, bl with
        | a::ax, b::bx -> f i a b; aux f (i+1) ax bx
        | a::ax, [] -> MP.report ~reference:(CWE 685) "Args" 
                        ("Not enough arguments for formatstring (need "^
                        (string_of_int (List.length (a::ax)))^" more)")
        | [], b::bx -> 
            MP.report ~reference:(CWE 685) "Args" "Too many arguments for formatstring"
        | _, _ -> ()
      in aux f 0 al bl
    in
    let typeOf arg = match Cil.isInteger arg with
      | Some i -> Multiple (
        List.fold_left (
          fun acc ik -> 
            if Cil.fitsInInt ik (Cilint.cilint_of_int64 i) then 
              (TInt (ik, []))::acc 
            else 
              acc
        ) [] cil_ikinds
      )
      | None -> Specific (Cil.typeOf arg)
    in
    let comp_types a b = 
        if b = Cil.charConstPtrType then (* A char const * is ok for formatting *)
          (Cil.typeSig a) = (Cil.typeSig Cil.charPtrType)
        else
          (Cil.typeSig a) = (Cil.typeSig b)
    in
    let (=|) a b = match b with
      | Specific t -> comp_types a t
      | Multiple tx -> List.exists (fun t -> comp_types a t) tx
    in
    let aux fsl al = 
      let atypes = List.map typeOf al in
      iteri2 (fun i ft at ->
        if not (ft =| at) then 
          MP.report ~reference:(CWE 688) "Type" ("Expected "^
                   (string_of_type ft)^" but got "^(string_of_comp_type at)^
                   " on index "^(string_of_int i))
        else 
          ()
      ) fsl atypes
    in aux fs arglist

  let buf_size_check ctx fs arglist =
    let ask_size x = match (ctx.ask (Q.EvalLength x)) with
      | `Int i -> Some i
      | _ -> match (ctx.ask (Q.BlobSize x)) with
        | `Int i -> Some i
        | _ -> None
    in
    let (-|) a b = match a, b with
      | Some i, Some j -> I64.sub i (I64.succ j)
      | _ -> I64.zero
    in
    let ftype = function
      | ScanSpecifier (_,_,_,t) -> t
      | _ -> ' '
    in
    let get_fsize = function
      | ScanSpecifier (None,Some x,_,_) -> Some (I64.of_int (~~ x))
      | _ -> None
    in
    let rec aux fl al = match fl, al with
      | fsp::fx, buf::ax -> 
          let fsize = get_fsize fsp in
          if (fsize = None) || ((ftype fsp) != 's' && (ftype fsp) != 'c') then
            aux fx ax
          else (
            let bsize = ask_size buf in
            let diff = bsize -| fsize in
            begin
              if (Cil.typeOf buf) = Cil.charConstPtrType then
                MP.report ~reference:(CERT "STR30-C") "Literal" ("Writing to "^(string_of_exp buf)^" with type "^(string_of_type (Cil.typeOf buf)))
              else if diff < I64.zero then
                MP.report "Buffer" ((string_of_exp buf)^" is too small (need "^(I64.to_string (I64.neg diff))^" more bytes)")
              else if bsize = None then 
                MP.report ~wclass:(MayWarn) "Buffer" ((string_of_exp buf)^" might be to small for input")
            end;
            aux fx ax
          )
      | _, _ -> ()
    in
    aux fs arglist

  let check_undelimited_string fs arglist scan =
    let arg i = try Some (List.nth arglist i) with _ -> None
    in
    let is_str_literal = function
      | Const (CStr _) -> true
      | _ -> false
    in
    let report () =
      if scan then 
        MP.report "Buffer" "The formatstring uses a %s without width which could lead to buffer overflow"
      else
        MP.report "Buffer" "The formatstring uses a %s without precision which could lead to buffer over-read"
    in
    let rec aux i = function
      | fsp::fx ->
          begin
            if is_undelimited_string fsp && not (is_str_literal (arg i |? Cil.zero)) then
              report ()
            else
              ()
          end;
          aux (i+1) fx
      | [] -> () 
    in aux 0 fs

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let is_scan_fun = function Scan _ -> true | _ -> false
    in
    let rec list_from i = function
      | x::xs -> if i = 0 then x :: (list_from i xs) else (list_from (i-1) xs)
      | [] -> []
    in
    begin
    let ftype = format_fun_type f.vname in
    let is = is_scan_fun ftype in
    match ftype with
      | Other -> ()
      | Print i | Scan i -> 
          match (ctx.ask (Q.EvalStr (List.nth arglist i))) with
          | `Str fs -> 
            let ax = list_from (i+1) arglist in
            let fp = parse_format_str fs is |> filter_nonformatting in
            let fx = format_args_types fp in
            type_check fx ax;
            check_undelimited_string fp ax is;
            if is then
              buf_size_check ctx fp ax
            else
              flag_check fp
          | _ -> 
              MP.report ~reference:(CWE 134) "ExtFmt" "Formatstring seems to be formed at runtime, which can lead to undefined behavior"
    end; 
    ctx.local

  let startstate v = D.bot ()
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  Messages.register_pool (module MP : Messages.Pool);
  MCP.register_analysis (module Spec : Spec)
