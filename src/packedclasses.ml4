(* 1 *)
DECLARE PLUGIN "packedclasses"

let () = Mltop.add_known_plugin (fun () ->
  Flags.if_verbose Pp.ppnl Pp.(str"packedclasses 0.1 loaded"))
  "packedclasses"
;;

(* 2 *)
open Glob_term
open Globnames
open Misctypes
open Evar_kinds
open Decl_kinds
open Names
open Proofview
open Pretyping
open Genarg
open Tacticals.New

(* 3 *)
let underscore = GHole(Loc.ghost, GoalEvar, IntroAnonymous, None)

let lambda id = GLambda(Loc.ghost, Name id, Explicit, underscore, underscore)

let myintro id : unit Proofview.tactic = Goal.nf_enter (fun g ->
  let env = Goal.env g in
  let goal = Goal.concl g in
  Refine.refine (fun sigma ->
    (* 4 *) ise_pretype_gen (default_inference_flags false) env sigma empty_lvar
      (OfType goal)
      (lambda id)
  ))
;;

let myintros ids = tclTHENLIST (List.map myintro ids)

TACTIC EXTEND packedclasses_intro
| [ "myintro" ident_list(ids) ] -> [ myintros ids ]
END

let myprint name : unit =
  (* 5 *) let reference = Smartlocate.global_with_alias name in
  match reference with
  | ConstRef c ->
     begin match Global.body_of_constant c with
     | Some b -> Pp.(msg_info (Printer.pr_constr b))
     | None -> Errors.errorlabstrm "packedclasses" Pp.(str "axiom")
     end
  | _ -> Errors.errorlabstrm "packedclasses" Pp.(str "can't print this")
;;

let newpackedstructure superclassname newclassname carrier args
		       mixinname : unit = 
  Pp.(msg_info ((str "Packing ") ++ (Libnames.pr_reference superclassname)));;
   

open G_rewrite
open Vernacexpr

type record_field = local_decl_expr with_instance with_priority with_notation

let wit_record_field =
 (Genarg.create_arg None "record_field" : record_field Genarg.uniform_genarg_type)

let record_field = Pcoq.create_generic_entry "record_field" (Genarg.rawwit wit_record_field)

open Pcoq

GEXTEND Gram
  GLOBAL: record_field;
    record_field:
    [ [ b = G_vernac.record_field -> b ] ];
END


VERNAC COMMAND EXTEND Packedclasses_print CLASSIFIED AS SIDEFF
| [ "Pack" "Structure" global(superclassname) ident(newclassname) 
	   binders(arguments)
  ":=" ident_opt(mixinname) "{" record_field_list_sep(mixinfields,";") "}"] -> [ 
    match List.rev arguments with
    | [] -> Errors.errorlabstrm "packedclasses" Pp
	    .(str "Please give at least one binder (the carrier).")
    | carrier :: revargs ->
       newpackedstructure superclassname newclassname
			  carrier (List.rev revargs) mixinname]
END

(* vim: set filetype=ocaml foldmethod=marker: *)
