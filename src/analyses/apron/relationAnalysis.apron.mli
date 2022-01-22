module M = Messages
module SpecFunctor :
  functor (CPriv : ApronPriv.S) (RD : RelationDomain.RD)
    (PCU : RelPrecCompareUtil.Util) ->
    sig
      type marshal = unit
      val init : 'a -> marshal
      val finalize : marshal -> marshal
      val call_descr : Cil.fundec -> 'a -> string
      val intrpt : ('a, 'b, 'c, 'd) Analyses.ctx -> 'a
      val vdecl : ('a, 'b, 'c, 'd) Analyses.ctx -> 'e -> 'a
      val asm : ('a, 'b, 'c, 'd) Analyses.ctx -> 'a
      val skip : ('a, 'b, 'c, 'd) Analyses.ctx -> 'a
      val morphstate : 'a -> 'b -> 'b
      module AD :
        sig
          type var = RD.Var.t
          type t = RD.D2.t
          type marshal = RD.D2.marshal
          val is_bot_env : t -> bool
          val equal : t -> t -> bool
          val hash : t -> int
          val compare : t -> t -> int
          val show : t -> string
          val pretty : marshal/2 -> t -> Pretty.doc
          val printXml : 'a BatInnerIO.output -> t -> marshal/2
          val name : marshal/2 -> string
          val to_yojson : t -> Printable.json
          val invariant : Invariant.context -> t -> Invariant.t
          val arbitrary : marshal/2 -> t QCheck.arbitrary
          val leq : t -> t -> bool
          val join : t -> t -> t
          val meet : t -> t -> t
          val widen : t -> t -> t
          val narrow : t -> t -> t
          val pretty_diff : marshal/2 -> t * t -> Pretty.doc
          val bot : marshal/2 -> t
          val is_bot : t -> bool
          val top : marshal/2 -> t
          val is_top : t -> bool
          val vars : t -> var list
          val add_vars : t -> var list -> t
          val remove_vars : t -> var list -> t
          val remove_filter : t -> (var -> bool) -> t
          val keep_vars : t -> var list -> t
          val keep_filter : t -> (var -> bool) -> t
          val forget_vars : t -> var list -> t
          val assign_exp : t -> var -> Cil.exp -> bool -> t
          val assign_var : t -> var -> var -> t
          val assign_var_parallel : t -> (var * var) list -> t
          val assign_var_parallel' : t -> var list -> var list -> t
          val substitute_exp : t -> var -> Cil.exp -> bool -> t
          val type_tracked : Cil.typ -> bool
          val varinfo_tracked : Cil.varinfo -> bool
          val assert_inv : t -> Cil.exp -> bool -> bool -> t
          val eval_int : t -> Cil.exp -> IntDomain.IntDomTuple.t
          val unify : t -> t -> t
          val tag : t -> int
          val relift : t -> t
          val marshal : t -> marshal
          val unmarshal : marshal -> t
        end
      val name : marshal -> string
      module Priv :
        sig
          module D :
            sig
              type t = CPriv(RD).D.t
              val equal : t -> t -> bool
              val hash : t -> int
              val compare : t -> t -> int
              val show : t -> string
              val pretty : marshal -> t -> Pretty.doc
              val printXml : 'a BatInnerIO.output -> t -> marshal
              val name : marshal -> string
              val to_yojson : t -> Printable.json
              val invariant : Invariant.context -> t -> Invariant.t
              val tag : t -> int
              val arbitrary : marshal -> t QCheck.arbitrary
              val relift : t -> t
              val leq : t -> t -> bool
              val join : t -> t -> t
              val meet : t -> t -> t
              val widen : t -> t -> t
              val narrow : t -> t -> t
              val pretty_diff : marshal -> t * t -> Pretty.doc
              val bot : marshal -> t
              val is_bot : t -> bool
              val top : marshal -> t
              val is_top : t -> bool
            end
          module G :
            sig
              type t = CPriv(RD).G.t
              val equal : t -> t -> bool
              val hash : t -> int
              val compare : t -> t -> int
              val show : t -> string
              val pretty : marshal -> t -> Pretty.doc
              val printXml : 'a BatInnerIO.output -> t -> marshal
              val name : marshal -> string
              val to_yojson : t -> Printable.json
              val invariant : Invariant.context -> t -> Invariant.t
              val tag : t -> int
              val arbitrary : marshal -> t QCheck.arbitrary
              val relift : t -> t
              val leq : t -> t -> bool
              val join : t -> t -> t
              val meet : t -> t -> t
              val widen : t -> t -> t
              val narrow : t -> t -> t
              val pretty_diff : marshal -> t * t -> Pretty.doc
              val bot : marshal -> t
              val is_bot : t -> bool
              val top : marshal -> t
              val is_top : t -> bool
            end
          module V :
            sig
              type t = CPriv(RD).V.t
              val equal : t -> t -> bool
              val hash : t -> int
              val compare : t -> t -> int
              val show : t -> string
              val pretty : marshal -> t -> Pretty.doc
              val printXml : 'a BatInnerIO.output -> t -> marshal
              val name : marshal -> string
              val to_yojson : t -> Printable.json
              val invariant : Invariant.context -> t -> Invariant.t
              val tag : t -> int
              val arbitrary : marshal -> t QCheck.arbitrary
              val relift : t -> t
            end
          val name : marshal -> string
          val startstate : marshal -> D.t
          val should_join :
            RelationDomain.RelComponent(RD.D2)(D).t ->
            RelationDomain.RelComponent(RD.D2)(D).t -> bool
          val read_global :
            Queries.ask ->
            (V.t -> G.t) ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            Cil.varinfo -> Cil.varinfo -> AD.t
          val write_global :
            ?invariant:bool ->
            Queries.ask ->
            (V.t -> G.t) ->
            (V.t -> G.t -> marshal) ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            Cil.varinfo ->
            Cil.varinfo -> RelationDomain.RelComponent(RD.D2)(D).t
          val lock :
            Queries.ask ->
            (V.t -> G.t) ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            LockDomain.Mutexes.elt -> RelationDomain.RelComponent(RD.D2)(D).t
          val unlock :
            Queries.ask ->
            (V.t -> G.t) ->
            (V.t -> G.t -> marshal) ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            LockDomain.Mutexes.elt -> RelationDomain.RelComponent(RD.D2)(D).t
          val sync :
            Queries.ask ->
            (V.t -> G.t) ->
            (V.t -> G.t -> marshal) ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            [ `Init | `Join | `Normal | `Return | `Thread ] ->
            RelationDomain.RelComponent(RD.D2)(D).t
          val enter_multithreaded :
            Queries.ask ->
            (V.t -> G.t) ->
            (V.t -> G.t -> marshal) ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            RelationDomain.RelComponent(RD.D2)(D).t
          val threadenter :
            Queries.ask ->
            (V.t -> G.t) ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            RelationDomain.RelComponent(RD.D2)(D).t
          val thread_join :
            Queries.ask ->
            (V.t -> G.t) ->
            Cil.exp ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            RelationDomain.RelComponent(RD.D2)(D).t
          val thread_return :
            Queries.ask ->
            (V.t -> G.t) ->
            (V.t -> G.t -> marshal) ->
            ThreadIdDomain.FlagConfiguredTID.t ->
            RelationDomain.RelComponent(RD.D2)(D).t ->
            RelationDomain.RelComponent(RD.D2)(D).t
          val init : marshal -> marshal
          val finalize : marshal -> marshal
        end
      module D :
        sig
          type t = (Priv.D.t, AD.t) RelationDomain.relcomponents_t
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val to_yojson : t -> Printable.json
          val _ : t -> Printable.json
          type group = Node.group = |
          val show_group : group -> 'a
          val to_group : 'a -> 'b option
          val trace_enabled : bool
          val tag : 'a -> 'b
          val relift : 'a -> 'a
          val hash : (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> int
          val show :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> string
          val pretty :
            marshal ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> Pretty.doc
          val printXml :
            'a BatInnerIO.output ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> marshal
          val name : marshal -> string
          val invariant :
            Invariant.context ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> Invariant.t
          val of_tuple : AD.t * Priv.D.t -> t
          val to_tuple : ('a, 'b) RelationDomain.relcomponents_t -> 'b * 'a
          val arbitrary : marshal -> t QCheck.arbitrary
          val bot :
            marshal -> (Priv.D.t, AD.t) RelationDomain.relcomponents_t
          val is_bot :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> bool
          val top :
            marshal -> (Priv.D.t, AD.t) RelationDomain.relcomponents_t
          val is_top :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> bool
          val leq :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> bool
          val pretty_diff : marshal -> t * t -> Pretty.doc
          val op_scheme :
            ('a -> 'b -> AD.t) ->
            ('c -> 'd -> Priv.D.t) ->
            ('c, 'a) RelationDomain.relcomponents_t ->
            ('d, 'b) RelationDomain.relcomponents_t -> t
          val join :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> t
          val meet :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> t
          val widen :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> t
          val narrow :
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
            (Priv.D.t, AD.t) RelationDomain.relcomponents_t -> t
        end
      module G = Priv.G
      module C = D
      module V = Priv.V
      module RV :
        sig
          module VH :
            sig
              type key = AD.var
              type 'a t = 'a BatHashtbl.Make(RD.Var).t
              val create : int -> 'a t
              val length : 'a t -> int
              val is_empty : 'a t -> bool
              val clear : 'a t -> marshal
              val copy : 'a t -> 'a t
              val add : 'a t -> key -> 'a -> marshal
              val remove : 'a t -> key -> marshal
              val remove_all : 'a t -> key -> marshal
              val find : 'a t -> key -> 'a
              val find_all : 'a t -> key -> 'a list
              val find_default : 'a t -> key -> 'a -> 'a
              val find_option : 'a t -> key -> 'a option
              val replace : 'a t -> key -> 'a -> marshal
              val mem : 'a t -> key -> bool
              val iter : (key -> 'a -> marshal) -> 'a t -> marshal
              val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
              val map : (key -> 'b -> 'c) -> 'b t -> 'c t
              val map_inplace : (key -> 'a -> 'a) -> 'a t -> marshal
              val filter : ('a -> bool) -> 'a t -> 'a t
              val filter_inplace : ('a -> bool) -> 'a t -> marshal
              val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
              val filteri_inplace : (key -> 'a -> bool) -> 'a t -> marshal
              val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
              val filter_map_inplace :
                (key -> 'a -> 'a option) -> 'a t -> marshal
              val modify : key -> ('a -> 'a) -> 'a t -> marshal
              val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> marshal
              val modify_opt :
                key -> ('a option -> 'a option) -> 'a t -> marshal
              val merge :
                (key -> 'a option -> 'b option -> 'c option) ->
                'a t -> 'b t -> 'c t
              val merge_all :
                (key -> 'a list -> 'b list -> 'c list) ->
                'a t -> 'b t -> 'c t
              val keys : 'a t -> key BatEnum.t
              val values : 'a t -> 'a BatEnum.t
              val enum : 'a t -> (key * 'a) BatEnum.t
              val to_list : 'a t -> (key * 'a) list
              val of_enum : (key * 'a) BatEnum.t -> 'a t
              val of_list : (key * 'a) list -> 'a t
              val print :
                ?first:string ->
                ?last:string ->
                ?sep:string ->
                ('a BatInnerIO.output -> key -> marshal) ->
                ('a BatInnerIO.output -> 'b -> marshal) ->
                'a BatInnerIO.output -> 'b t -> marshal
              module Exceptionless :
                sig
                  val find : 'a t -> key -> 'a option
                  val modify :
                    key -> ('a -> 'a) -> 'a t -> (marshal, exn) result
                end
              module Infix :
                sig
                  val ( --> ) : 'a t -> key -> 'a
                  val ( <-- ) : 'a t -> key * 'a -> marshal
                end
              module Labels :
                sig
                  val add : 'a t -> key:key -> data:'a -> marshal
                  val replace : 'a t -> key:key -> data:'a -> marshal
                  val iter :
                    f:(key:key -> data:'a -> marshal) -> 'a t -> marshal
                  val map : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
                  val map_inplace :
                    f:(key:key -> data:'a -> 'a) -> 'a t -> marshal
                  val filter : f:('a -> bool) -> 'a t -> 'a t
                  val filter_inplace : f:('a -> bool) -> 'a t -> marshal
                  val filteri :
                    f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
                  val filteri_inplace :
                    f:(key:key -> data:'a -> bool) -> 'a t -> marshal
                  val filter_map :
                    f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
                  val filter_map_inplace :
                    f:(key:key -> data:'a -> 'a option) -> 'a t -> marshal
                  val fold :
                    f:(key:key -> data:'a -> 'b -> 'b) ->
                    'a t -> init:'b -> 'b
                  val modify : key:key -> f:('a -> 'a) -> 'a t -> marshal
                  val modify_def :
                    default:'a -> key:key -> f:('a -> 'a) -> 'a t -> marshal
                  val modify_opt :
                    key:key -> f:('a option -> 'a option) -> 'a t -> marshal
                  val merge :
                    f:(key -> 'a option -> 'b option -> 'c option) ->
                    left:'a t -> right:'b t -> 'c t
                  val merge_all :
                    f:(key -> 'a list -> 'b list -> 'c list) ->
                    left:'a t -> right:'b t -> 'c t
                end
            end
          val vh : RelationDomain.RelVM.t VH.t
          val make_var : ?name:string -> RelationDomain.RelVM.t -> VH.key
          val find_metadata : VH.key -> RelationDomain.RelVM.t option
          val local : Cil.varinfo -> VH.key
          val arg : Cil.varinfo -> VH.key
          val return : VH.key
          val global : Cil.varinfo -> VH.key
        end
      module PCU :
        sig
          module Key :
            sig
              type t = Node.t
              val equal : t -> t -> bool
              val hash : t -> int
              val compare : t -> t -> int
              val show : t -> string
              val pretty : marshal -> t -> Pretty.doc
              val printXml : 'a BatInnerIO.output -> t -> marshal
              val name : marshal -> string
              val to_yojson : t -> Printable.json
              val invariant : Invariant.context -> t -> Invariant.t
              val tag : t -> int
              val arbitrary : marshal -> t QCheck.arbitrary
              val relift : t -> t
              val to_location : t -> Cil.location
            end
          module Dom :
            sig
              type t = AD.t
              val equal : t -> t -> bool
              val hash : t -> int
              val compare : t -> t -> int
              val show : t -> string
              val pretty : marshal -> t -> Pretty.doc
              val printXml : 'a BatInnerIO.output -> t -> marshal
              val name : marshal -> string
              val to_yojson : t -> Printable.json
              val invariant : Invariant.context -> t -> Invariant.t
              val tag : t -> int
              val arbitrary : marshal -> t QCheck.arbitrary
              val relift : t -> t
              val leq : t -> t -> bool
              val join : t -> t -> t
              val meet : t -> t -> t
              val widen : t -> t -> t
              val narrow : t -> t -> t
              val pretty_diff : marshal -> t * t -> Pretty.doc
              val bot : marshal -> t
              val is_bot : t -> bool
              val top : marshal -> t
              val is_top : t -> bool
            end
          module RH :
            sig
              type key = Node.t
              type 'a t =
                  'a PrecCompareUtil.Util(RelPrecCompareUtil.MyNode)(AD).RH.t
              val create : int -> 'a t
              val length : 'a t -> int
              val is_empty : 'a t -> bool
              val clear : 'a t -> marshal
              val copy : 'a t -> 'a t
              val add : 'a t -> key -> 'a -> marshal
              val remove : 'a t -> key -> marshal
              val remove_all : 'a t -> key -> marshal
              val find : 'a t -> key -> 'a
              val find_all : 'a t -> key -> 'a list
              val find_default : 'a t -> key -> 'a -> 'a
              val find_option : 'a t -> key -> 'a option
              val replace : 'a t -> key -> 'a -> marshal
              val mem : 'a t -> key -> bool
              val iter : (key -> 'a -> marshal) -> 'a t -> marshal
              val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
              val map : (key -> 'b -> 'c) -> 'b t -> 'c t
              val map_inplace : (key -> 'a -> 'a) -> 'a t -> marshal
              val filter : ('a -> bool) -> 'a t -> 'a t
              val filter_inplace : ('a -> bool) -> 'a t -> marshal
              val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
              val filteri_inplace : (key -> 'a -> bool) -> 'a t -> marshal
              val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
              val filter_map_inplace :
                (key -> 'a -> 'a option) -> 'a t -> marshal
              val modify : key -> ('a -> 'a) -> 'a t -> marshal
              val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> marshal
              val modify_opt :
                key -> ('a option -> 'a option) -> 'a t -> marshal
              val merge :
                (key -> 'a option -> 'b option -> 'c option) ->
                'a t -> 'b t -> 'c t
              val merge_all :
                (key -> 'a list -> 'b list -> 'c list) ->
                'a t -> 'b t -> 'c t
              val keys : 'a t -> key BatEnum.t
              val values : 'a t -> 'a BatEnum.t
              val enum : 'a t -> (key * 'a) BatEnum.t
              val to_list : 'a t -> (key * 'a) list
              val of_enum : (key * 'a) BatEnum.t -> 'a t
              val of_list : (key * 'a) list -> 'a t
              val print :
                ?first:string ->
                ?last:string ->
                ?sep:string ->
                ('a BatInnerIO.output -> key -> marshal) ->
                ('a BatInnerIO.output -> 'b -> marshal) ->
                'a BatInnerIO.output -> 'b t -> marshal
              module Exceptionless :
                sig
                  val find : 'a t -> key -> 'a option
                  val modify :
                    key -> ('a -> 'a) -> 'a t -> (marshal, exn) result
                end
              module Infix :
                sig
                  val ( --> ) : 'a t -> key -> 'a
                  val ( <-- ) : 'a t -> key * 'a -> marshal
                end
              module Labels :
                sig
                  val add : 'a t -> key:key -> data:'a -> marshal
                  val replace : 'a t -> key:key -> data:'a -> marshal
                  val iter :
                    f:(key:key -> data:'a -> marshal) -> 'a t -> marshal
                  val map : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
                  val map_inplace :
                    f:(key:key -> data:'a -> 'a) -> 'a t -> marshal
                  val filter : f:('a -> bool) -> 'a t -> 'a t
                  val filter_inplace : f:('a -> bool) -> 'a t -> marshal
                  val filteri :
                    f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
                  val filteri_inplace :
                    f:(key:key -> data:'a -> bool) -> 'a t -> marshal
                  val filter_map :
                    f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
                  val filter_map_inplace :
                    f:(key:key -> data:'a -> 'a option) -> 'a t -> marshal
                  val fold :
                    f:(key:key -> data:'a -> 'b -> 'b) ->
                    'a t -> init:'b -> 'b
                  val modify : key:key -> f:('a -> 'a) -> 'a t -> marshal
                  val modify_def :
                    default:'a -> key:key -> f:('a -> 'a) -> 'a t -> marshal
                  val modify_opt :
                    key:key -> f:('a option -> 'a option) -> 'a t -> marshal
                  val merge :
                    f:(key -> 'a option -> 'b option -> 'c option) ->
                    left:'a t -> right:'b t -> 'c t
                  val merge_all :
                    f:(key -> 'a list -> 'b list -> 'c list) ->
                    left:'a t -> right:'b t -> 'c t
                end
            end
          type marshal = PCU(AD).marshal
          type dump = marshal PrecCompareUtil.dump_gen
          type result = Dom.t RH.t PrecCompareUtil.result_gen
          val init : marshal/2 -> marshal/2
          val unmarshal : marshal -> Dom.t RH.t
        end
      val results : AD.t PCU.RH.t
      val should_join :
        RelationDomain.RelComponent(RD.D2)(Priv.D).t ->
        RelationDomain.RelComponent(RD.D2)(Priv.D).t -> bool
      val context :
        Cil.fundec ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t
      val exitstate : 'a -> (Priv.D.t, AD.t) RelationDomain.relcomponents_t
      val startstate : 'a -> (Priv.D.t, AD.t) RelationDomain.relcomponents_t
      val read_global :
        Queries.ask ->
        (V.t -> G.t) ->
        RelationDomain.RelComponent(RD.D2)(Priv.D).t ->
        Cil.varinfo -> Cil.varinfo -> AD.t
      module VH :
        sig
          type key = Cil.varinfo
          type 'a t = 'a BatHashtbl.Make(Basetype.Variables).t
          val create : int -> 'a t
          val length : 'a t -> int
          val is_empty : 'a t -> bool
          val clear : 'a t -> marshal
          val copy : 'a t -> 'a t
          val add : 'a t -> key -> 'a -> marshal
          val remove : 'a t -> key -> marshal
          val remove_all : 'a t -> key -> marshal
          val find : 'a t -> key -> 'a
          val find_all : 'a t -> key -> 'a list
          val find_default : 'a t -> key -> 'a -> 'a
          val find_option : 'a t -> key -> 'a option
          val replace : 'a t -> key -> 'a -> marshal
          val mem : 'a t -> key -> bool
          val iter : (key -> 'a -> marshal) -> 'a t -> marshal
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val map : (key -> 'b -> 'c) -> 'b t -> 'c t
          val map_inplace : (key -> 'a -> 'a) -> 'a t -> marshal
          val filter : ('a -> bool) -> 'a t -> 'a t
          val filter_inplace : ('a -> bool) -> 'a t -> marshal
          val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
          val filteri_inplace : (key -> 'a -> bool) -> 'a t -> marshal
          val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
          val filter_map_inplace :
            (key -> 'a -> 'a option) -> 'a t -> marshal
          val modify : key -> ('a -> 'a) -> 'a t -> marshal
          val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> marshal
          val modify_opt : key -> ('a option -> 'a option) -> 'a t -> marshal
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val merge_all :
            (key -> 'a list -> 'b list -> 'c list) -> 'a t -> 'b t -> 'c t
          val keys : 'a t -> key BatEnum.t
          val values : 'a t -> 'a BatEnum.t
          val enum : 'a t -> (key * 'a) BatEnum.t
          val to_list : 'a t -> (key * 'a) list
          val of_enum : (key * 'a) BatEnum.t -> 'a t
          val of_list : (key * 'a) list -> 'a t
          val print :
            ?first:string ->
            ?last:string ->
            ?sep:string ->
            ('a BatInnerIO.output -> key -> marshal) ->
            ('a BatInnerIO.output -> 'b -> marshal) ->
            'a BatInnerIO.output -> 'b t -> marshal
          module Exceptionless :
            sig
              val find : 'a t -> key -> 'a option
              val modify : key -> ('a -> 'a) -> 'a t -> (marshal, exn) result
            end
          module Infix :
            sig
              val ( --> ) : 'a t -> key -> 'a
              val ( <-- ) : 'a t -> key * 'a -> marshal
            end
          module Labels :
            sig
              val add : 'a t -> key:key -> data:'a -> marshal
              val replace : 'a t -> key:key -> data:'a -> marshal
              val iter : f:(key:key -> data:'a -> marshal) -> 'a t -> marshal
              val map : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
              val map_inplace :
                f:(key:key -> data:'a -> 'a) -> 'a t -> marshal
              val filter : f:('a -> bool) -> 'a t -> 'a t
              val filter_inplace : f:('a -> bool) -> 'a t -> marshal
              val filteri : f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
              val filteri_inplace :
                f:(key:key -> data:'a -> bool) -> 'a t -> marshal
              val filter_map :
                f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
              val filter_map_inplace :
                f:(key:key -> data:'a -> 'a option) -> 'a t -> marshal
              val fold :
                f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
              val modify : key:key -> f:('a -> 'a) -> 'a t -> marshal
              val modify_def :
                default:'a -> key:key -> f:('a -> 'a) -> 'a t -> marshal
              val modify_opt :
                key:key -> f:('a option -> 'a option) -> 'a t -> marshal
              val merge :
                f:(key -> 'a option -> 'b option -> 'c option) ->
                left:'a t -> right:'b t -> 'c t
              val merge_all :
                f:(key -> 'a list -> 'b list -> 'c list) ->
                left:'a t -> right:'b t -> 'c t
            end
        end
      val read_globals_to_locals :
        Queries.ask ->
        (V.t -> G.t) ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
        Cil.exp -> AD.t * Cil.exp * Cil.varinfo VH.t
      val read_from_globals_wrapper :
        Queries.ask ->
        (V.t -> G.t) ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
        Cil.exp -> (AD.t -> Cil.exp -> 'a) -> 'a
      val assign_from_globals_wrapper :
        Queries.ask ->
        (V.t -> G.t) ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
        Cil.exp -> (AD.t -> Cil.exp -> AD.t) -> AD.t
      val write_global :
        Queries.ask ->
        (V.t -> G.t) ->
        (V.t -> G.t -> marshal) ->
        RelationDomain.RelComponent(RD.D2)(Priv.D).t ->
        Cil.varinfo ->
        Cil.varinfo -> RelationDomain.RelComponent(RD.D2)(Priv.D).t
      val assign_to_global_wrapper :
        Queries.ask ->
        (V.t -> G.t) ->
        (V.t -> G.t -> marshal) ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
        Cil.lhost * Cil.offset ->
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
         Cil.varinfo -> AD.t) ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t
      val no_overflow : ('a, 'b, 'c, 'd) Analyses.ctx -> Cil.exp -> bool
      val assert_type_bounds :
        AD.t -> Cil.varinfo -> ('a, 'b, 'c, 'd) Analyses.ctx -> AD.t
      val assign :
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t, G.t, 'a, V.t)
        Analyses.ctx ->
        Prelude.Ana.lval ->
        Cil.exp -> (Priv.D.t, AD.t) RelationDomain.relcomponents_t
      val branch :
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t, G.t, 'a, V.t)
        Analyses.ctx ->
        Cil.exp -> bool -> (Priv.D.t, AD.t) RelationDomain.relcomponents_t
      val enter :
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t, G.t, 'a, V.t)
        Analyses.ctx ->
        'b ->
        Cil.fundec ->
        Cil.exp list ->
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t *
         (Priv.D.t, AD.t) RelationDomain.relcomponents_t)
        list
      val body :
        (('a, AD.t) RelationDomain.relcomponents_t, 'b, 'c, 'd) Analyses.ctx ->
        Cil.fundec -> ('a, AD.t) RelationDomain.relcomponents_t
      val return :
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t, G.t, 'a, V.t)
        Analyses.ctx ->
        Cil.exp option ->
        Cil.fundec -> RelationDomain.RelComponent(RD.D2)(Priv.D).t
      val combine :
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t, G.t, 'a, V.t)
        Analyses.ctx ->
        (Cil.lhost * Cil.offset) option ->
        'b ->
        Cil.fundec ->
        Cil.exp list ->
        'c ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t ->
        (Priv.D.t, AD.t) RelationDomain.relcomponents_t
      val special :
        (RelationDomain.RelComponent(RD.D2)(Priv.D).t, G.t, 'a, V.t)
        Analyses.ctx ->
        (Cil.lhost * Cil.offset) option ->
        Cil.varinfo ->
        Cil.exp list -> RelationDomain.RelComponent(RD.D2)(Priv.D).t
      val query :
        ((Priv.D.t, AD.t) RelationDomain.relcomponents_t, G.t, 'b, V.t)
        Analyses.ctx -> 'a Queries.t -> 'a
      val threadenter :
        (RelationDomain.RelComponent(RD.D2)(Priv.D).t, G.t, 'a, V.t)
        Analyses.ctx ->
        'b ->
        Cil.varinfo ->
        'c -> (Priv.D.t, AD.t) RelationDomain.relcomponents_t list
      val threadspawn :
        ('a, 'b, 'c, 'd) Analyses.ctx -> 'e -> 'f -> 'g -> 'h -> 'a
      val event :
        (RelationDomain.RelComponent(RD.D2)(Priv.D).t, 'a, 'b, 'c)
        Analyses.ctx ->
        Events.t ->
        ('d, G.t, 'e, V.t) Analyses.ctx ->
        RelationDomain.RelComponent(RD.D2)(Priv.D).t
      val sync :
        (RelationDomain.RelComponent(RD.D2)(Priv.D).t, G.t, 'a, V.t)
        Analyses.ctx ->
        [< `Init | `Join | `Normal | `Return | `Thread ] ->
        RelationDomain.RelComponent(RD.D2)(Priv.D).t
    end
