(* == CakeML == *)
open preamble;

(* open the relevant theories in your development *)
open separationLogicTheory;

val _ = new_theory "pp";

(* Useful term to hide stuff *)
val OMIT_def = Define `OMIT x = x`
val HIDE_def = Define `HIDE x = x`

val _ = export_theory();
