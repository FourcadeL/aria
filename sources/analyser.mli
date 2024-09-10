(* The analyser serve the first pass checking BEFORE compilation
static checking is done on the AST, Warning and errors are raised
if uncountered*)
open Ast

(*---------------------------------------------------------------*)
(*---------------------- analyser functions ---------------------*)
(*---------------------------------------------------------------*)

val analyser_check : globalAst -> unit