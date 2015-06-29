

Drop. Drop. () ;;

#use "include" ;;

let import_plugin = parse_vernac "Require Import packedclasses.";;
let import_string = parse_vernac "Require Import Strings.String.";;

let ghost_eval_expr x = eval_expr (Loc.ghost, x);;

ghost_eval_expr import_plugin;;
ghost_eval_expr import_string;;

parse_vernac "Module Structure.";;

ghost_eval_expr (parse_vernac "Module Structure.");;

parse_vernac "Structure interface := Interface {  carrier_type :> Type;  name : string;  class : carrier_type -> Type;  mixin : carrier_type -> Type;  parents : list carrier_type}.")

ghost_eval_expr (parse_vernac "Structure interface := Interface {  carrier_type :> Type;  name : string;  class : carrier_type -> Type;  mixin : carrier_type -> Type;  parents : list carrier_type}.") ;;

ghost_eval_expr (parse_vernac "End Structure.") ;;




(* Require Import packedclasses. *)
(* Require Import Strings.String. *)

(* Module Structure. *)

(* Structure interface := Interface { *)
(*   carrier_type :> Type; *)
(*   name : string; *)
(*   class : carrier_type -> Type; *)
(*   (\* mixin : carrier_type -> Type; *\) *)
(*   parents : list carrier_type *)
(* }. *)

(* End Structure. *)



(* Pack Structure nat test (T : Type) := *)
(*   Foo {foo : T -> T; bar : forall x, foo x = x}. *)

