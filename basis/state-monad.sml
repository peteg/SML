(* State monad. *)
structure StateM =
struct

type ('a, 's) m = 's -> 'a * 's

fun bind (f: ('a, 's) m) (g: 'a -> ('b, 's) m) : ('b, 's) m = uncurry g o f
fun return (x: 'a) : ('a, 's) m = fn s => (x, s)
fun map (f: 'a -> 'b) (t: ('a, 's) m) : ('b, 's) m = Pair.mapFst f o t
fun map2 (f: 'a -> 'b -> 'c) (t: ('a, 's) m) (u: ('b, 's) m) : ('c, 's) m =
  bind t (fn t => bind u (fn u => return (f t u)))
fun fold (f: 'a * 'b -> ('b, 's) m) (b: 'b) : 'a list -> ('b, 's) m =
  fn [] => return b
  | x :: xs => bind (f (x, b)) (fn b' => fold f b' xs)

(* FIXME perhaps move to a StateM_syn module? *)
val op >>= = bind

end
