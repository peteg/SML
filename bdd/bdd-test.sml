(* FIXME test code *)
open Bdd;

val a = var 0;
val b = var 1;
val c = var 2;

val bdd = xor (a, b);

val l = toDot bdd;
Layout.print (l, print);
print "\n";
