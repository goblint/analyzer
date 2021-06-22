# Your first analysis

This is a tutorial exercise for writing analyses in Goblint.
We will implement a very simple sign analysis.
You need to install goblint for development.

## First test

Create a C file in the Goblint root directory for testing.
Here we call it `example.c`. This is the first test:


```
#include<assert.h>

int main() {

  int x;
  int unknown;

  if (unknown) {
    x = 5;
  } else {
    x = 7;
  }

  // The above code branches on an uninitialized variable.
  // The value of x could be either 5 or 7.

  assert(x > 0);  // Thus, this assertion should hold!

  return 0;
}
```

If you run goblint out of the box on this example it will not work: `./goblint --enable dbg.debug example.c` will claim that the assertion in unknown.
Goblint could verify that this asertion does hold using interval analysis (`--enable ana.int.interval`), but here we will implement a simple sign analysis instead.

We begin by registering and running a new analysis called "signs":

1. Create a copy of the unit analysis (*src/analyses/unit.ml*) into the file *src/analyses/signs.ml*.
2. Inside the file, change the name from "unit" to "signs".
3. Try to run goblint with the new analysis enabled: `./goblint --sets "ana.activated[+]" signs --enable dbg.debug example.c`. The result will still be that nothing is verified.
4. Now edit the analysis to pass the test. :)


## Designing the domain

The last step might need some clarifications. We first need to design the abstract domain. It may help if you have read some theoretical tutorial on abstract domains. Our first sign lattice will simply contain the elements `{-, 0, +}` with top and bottom added. For this, we define first the signs and then we lift it. Insert the following into your file just before the module `Spec` is defined.

```
module Signs =
struct
  type t = Neg | Zero | Pos [@@deriving eq, ord, to_yojson]
  let name () = "signs"
  let show x = match x with
  | Neg -> "-"
  | Pos -> "+"
  | Zero -> "0"

  (* We need the following to generate output... *)
  include Printable.Std
  include Printable.PrintSimple (struct
    type t' = t
    let name = name
    let show = show
  end)
  let hash = Hashtbl.hash

  (* Here as important domain-specific implementations,
   * there are some mistakes for you to fix... *)
  let of_int i =
    if i < Int64.zero then Zero
    else if i > Int64.zero then Zero
    else Zero

  let gt x y = match x,y with
  | Pos, Neg | Zero, Neg -> true
  | _ -> false

end

module SL =
struct
  include Lattice.Flat (Signs) (Printable.DefaultNames)
  let of_int i = `Lifted (Signs.of_int i)

  let gt x y = match x, y with
  | `Lifted x, `Lifted y -> Signs.gt x y
  | _ -> false
end
```

There are some flawed definitions above to get you started, but let us first tell the analysis to actually use this domain.
For that, we change the similar lines that currently use the unit domain to now use a map from variables to the newly created sign domains.

```
module D = MapDomain.MapBot (Basetype.Variables) (SL)
module G = Lattice.Unit
module C = D
```

We will not care about the globals and the context, but it's convenient to have the same context as the domain itself.


## Implementing the sign analysis

The key part now is to define transfer functions for assignment. Here is a simple version that will suffice for our example. Replace the definition of `assign` in your file with the following:

```
let eval (d: D.t) (exp: exp): SL.t = match exp with
| Const (CInt64 (i, _, _)) -> SL.of_int i
| Lval (Var x, NoOffset) -> D.find x d
| _ -> SL.top ()

let assign ctx (lval:lval) (rval:exp) : D.t =
  let d = ctx.local in
  match lval with
  | Var x, NoOffset -> D.add x (eval d rval) d
  | _ -> D.top ()
```

We only handle assignments of the form `x = e` where the right-hand side `e` is itself either a constant of type integer or a simple variable. For this to work, the definition of `SL.of_int` needs to be corrected to pick an appropriate sign abstraction of integers. There is no need to implement the transfer functions for branching for this example; it only relies on lattice join operations to correctly take both paths into account.

At this point, it may be useful to use Goblint's HTML output to [see the result](../user-guide/inspecting.md) of the analysis. This will also include Goblnt's base analysis, which is needed to deal with function calls, but it may be useful to turn off integer analysis with `--disable int.ana.def_exc`, so that only your implemented analysis plays a role.

## Checking assertions

With this in place, we should have sufficient information to tell Goblint that the assertion does hold. We need to answer assertion queries as follows:

```
let assert_holds (d: D.t) (e:exp) = match e with
| BinOp (Gt, e1, e2, t) -> SL.gt (eval d e1) (eval d e2)
| _ -> false

let query ctx (type a) (q: a Queries.t): a Queries.result =
  let open Queries in match q with
  | Assert e when assert_holds ctx.local e -> `Lifted true
  | _ -> Result.top q
```

This again does the bare minimum for the above test case to work, assuming that the definition of `SL.gt` is correct. Goblint's base analysis query's other analyses when evaluating assertions and will use the best information available. It is, therefore, important that we only give a definite response (``Lifted true`) when the assertion will always hold.

You could now independently enrich the lattice to also keep track of non-negative and non-positive values, such that the join of Zero and Pos is NonNeg, and then handle assertions for `x >= 0`.