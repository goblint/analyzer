(** The C library's standard streams [stdin], [stdout] and [stderr].

    The C standard declares each as an expression of type [FILE *] that points
    to a stream object owned by the library; glibc declares each as an
    [extern FILE *] variable of the same name, and Darwin as one named
    [__stdoutp] and so on ({!of_name}). Where a C library names them
    otherwise, the stream expressions of the program are not recognized and
    hold unknown addresses, so every call through them reports writing
    through an unknown address, while a function such as [printf] that uses a
    stream without taking it as an argument uses the stream's object. The analysis gives each stream
    an object of its own, a global variable named ["[stdout]"] and so on (see
    {!object_var}), and at program start sets the declared variable to its
    address. Nothing in the C library assigns these variables afterwards, so
    unlike other [extern] variables they are not treated as changing
    unpredictably ({!BaseUtil.is_always_unknown}); an assignment by the program
    itself is analyzed as any other.

    {b What the object holds.} The only part of a stream's state that is
    program memory is the buffer that [setvbuf], [setbuf] or [setbuffer]
    handed it, which later stream functions read and write until the stream
    is closed or the program exits. The object is therefore a [void *] holding
    that buffer, or null while the stream uses a buffer of the library's own;
    the objects are initialized to null wherever the file uses the standard
    streams ({!LibraryFunctions.standard_streams_used}), declared or not. Only
    those three functions store into an object
    ({!LibraryDesc.special.SetStreamBuffer}): they add to what it holds, except
    that giving one definite stream no buffer replaces it with null.

    {b What a library call does to it.} A library function with a
    specification in {!LibraryFunctions} that reads or writes through a stream,
    however deep its specification says, writes the bytes of the buffer the
    object holds, and nothing the buffer's contents point to, and leaves the
    object itself as it is ([Base.invalidate] with [keep_streams]). A function
    that uses a stream without taking it as an argument, as [printf] uses
    [stdout], carries {!LibraryDesc.attr.UsesStream} and does the same to that
    stream, and [fflush] given a stream that may be null
    ({!LibraryDesc.attr.AllStreamsIfNull}) to all three. Each access to a buffer is reported as an [Access] event of its
    own ([AccessAnalysis.do_access]), so [useAfterFree] checks it, and the
    race analysis takes it as made under the lock on the stream
    ([MutexAnalysis.access]), which is the stream's object, as [flockfile]
    takes it -- except for a function with {!LibraryDesc.attr.StreamUnlocked},
    such as [putc_unlocked], which takes no lock. The objects themselves are
    never data races ({!Access.is_ignorable_mval}): only the three functions
    above write one, under the lock.

    {b Functions without a specification.} Such a function may assign the
    variables and hand any stream any buffer. Where it invalidates the
    globals, as by default, it sets the declared stream variables and the
    objects to unknown values, so a later stream function writes through an
    unknown address and says so. Where it does not, a stream object reachable
    from its arguments comes to hold every object the function reaches.

    {b The buffer's lifetime.} The C standard requires the buffer to outlive
    its use by the stream. The analysis does not check this: at each call that
    attaches a buffer that may be storage ending before the program exits -- a
    local variable, thread-local storage, [alloca] or heap memory, or an
    unknown address -- it reports the assumption {!buffer_lifetime}. *)

open GoblintCil

type t = Stdin | Stdout | Stderr

let all = [Stdin; Stdout; Stderr]

let name = function
  | Stdin -> "stdin"
  | Stdout -> "stdout"
  | Stderr -> "stderr"

(** The stream a C library's variable of this name is: glibc and musl name
    them [stdin], [stdout] and [stderr]; Darwin's [<stdio.h>] declares
    [__stdinp], [__stdoutp] and [__stderrp] and defines the standard names as
    macros for them. *)
let of_name = function
  | "stdin" | "__stdinp" -> Some Stdin
  | "stdout" | "__stdoutp" -> Some Stdout
  | "stderr" | "__stderrp" -> Some Stderr
  | _ -> None

(** Attribute marking the object of a standard stream. *)
let attribute = "goblint_standard_stream"

let objects: (t, varinfo) Hashtbl.t = Hashtbl.create 3

(** The object of the stream [s], a global of type [void *] named ["[stdout]"]
    for [stdout]. Created once per run. *)
let object_var s =
  match Hashtbl.find_opt objects s with
  | Some v -> v
  | None ->
    let v = Cilfacade.create_var (makeGlobalVar ("[" ^ name s ^ "]") voidPtrType) in
    v.vattr <- [Attr (attribute, [])];
    Hashtbl.replace objects s v;
    v

(** Whether [v] is the object of a standard stream. *)
let is_object v = hasAttribute attribute v.vattr

(** The pointer-typed globals of the analyzed file named after a standard
    stream that the program may assign: it defines the variable, an
    assignment or a call's result stores into it, or its address is taken. *)
let assigned_variables: (t * varinfo) list ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      let assigned = ref [] in
      let add v =
        match of_name v.vname with
        | Some s when v.vglob && isPointerType v.vtype && not (List.mem_assoc s !assigned) -> assigned := (s, v) :: !assigned
        | _ -> ()
      in
      let visitor = object
        inherit nopCilVisitor
        method! vinst = function
          | Set ((Var v, _), _, _, _) | Call (Some (Var v, _), _, _, _, _) -> add v; DoChildren
          | _ -> DoChildren
        method! vvrbl v = if v.vaddrof then add v; SkipChildren
        method! vglob = function
          | GVar (v, _, _) -> add v; DoChildren
          | _ -> DoChildren
      end
      in
      visitCilFileSameGlobals visitor !Cilfacade.current_file;
      !assigned
    )

(** The pointer-typed globals of the analyzed file named after a standard
    stream, declared or defined. *)
let declared_variables: (t * varinfo) list ResettableLazy.t =
  ResettableLazy.from_fun (fun () ->
      foldGlobals !Cilfacade.current_file (fun acc -> function
          | GVarDecl (v, _) | GVar (v, _, _) when isPointerType v.vtype ->
            begin match of_name v.vname with
              | Some s when not (List.mem_assoc s acc) -> (s, v) :: acc
              | _ -> acc
            end
          | _ -> acc
        ) []
    )

let declared_variable s = List.assoc_opt s (ResettableLazy.force declared_variables)

(** The stream that [v] names if [v] is an [extern] pointer variable named
    [stdin], [stdout] or [stderr], as glibc's [<stdio.h>] declares them. When
    the analyzed file declares such a variable and does not define it, the
    variable holds the address of [object_var s] at program start. *)
let of_extern_variable v =
  if v.vstorage = Extern && isPointerType v.vtype then of_name v.vname else None

(** The expression through which a library function that uses [s] without
    taking it as an argument, as [printf] uses [stdout], reaches the stream:
    the file's variable [stdout] if the program may assign it, and the address
    of the stream's object otherwise, which is the value that variable then
    always holds. *)
let exp s =
  match declared_variable s with
  | Some v -> Lval (Var v, NoOffset)
  | None -> AddrOf (Var (object_var s), NoOffset)

(** Whether the program may assign the variable of [s]. *)
let assigned s = List.mem_assoc s (ResettableLazy.force assigned_variables)

(** The expression of the access a library call makes to the buffer the
    stream object [v] holds: [Lval] of [v] where the call takes the lock on
    the stream, as every stdio function does except the [*_unlocked] ones,
    and [*&v] where it does not. *)
let buffer_exp ~locked v =
  if locked then Lval (Var v, NoOffset)
  else Lval (Mem (AddrOf (Var v, NoOffset)), NoOffset)

(** The stream object [e] names if [e] is the expression of a buffer access
    made under the stream's lock ({!buffer_exp}). *)
let locked_buffer = function
  | Lval (Var v, NoOffset) when is_object v -> Some v
  | _ -> None

(** Assumption reported at a [setvbuf] or [setbuf] whose buffer may be storage
    that ends before the program exits, which the analysis does not check. *)
let buffer_lifetime =
  "libc.stream-buffer-lifetime: a buffer attached to a stream with setvbuf or setbuf stays live until the stream is closed or the program exits"

(** Reports [buffer_lifetime] at the current node. *)
let buffer_may_end () =
  Assumptions.add ~final:("Used assumption " ^ buffer_lifetime) "%s" buffer_lifetime

let reset_lazy () =
  ResettableLazy.reset assigned_variables;
  ResettableLazy.reset declared_variables
