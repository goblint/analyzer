
class omit_post_solving =
object(self)
val mutable omitPostSolving : bool = false

method setFlag () =
  omitPostSolving <- true

method getFlag () =
  omitPostSolving
end

let omitPostSolving = new omit_post_solving