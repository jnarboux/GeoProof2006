(* ========================================================================= *)
(* Polymorphic type of formulas with parser and printer.                     *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

open Lib
open Format

type ('a)formula = False
                 | True
                 | Atom of 'a
                 | Not of ('a)formula
                 | And of ('a)formula * ('a)formula
                 | Or of ('a)formula * ('a)formula
                 | Imp of ('a)formula * ('a)formula
                 | Iff of ('a)formula * ('a)formula
                 | Forall of string * ('a)formula
                 | Exists of string * ('a)formula;;

(* ------------------------------------------------------------------------- *)
(* General homomorphism and iteration functions for atoms in formula.        *)
(* ------------------------------------------------------------------------- *)

let rec onatoms fn fm =
  match fm with
    Atom(a) -> fn a
  | Not(p) -> Not(onatoms fn p)
  | And(p,q) -> And(onatoms fn p,onatoms fn q)
  | Or(p,q) -> Or(onatoms fn p,onatoms fn q)
  | Imp(p,q) -> Imp(onatoms fn p,onatoms fn q)
  | Iff(p,q) -> Iff(onatoms fn p,onatoms fn q)
  | Forall(x,p) -> Forall(x,onatoms fn p)
  | Exists(x,p) -> Exists(x,onatoms fn p)
  | _ -> fm;;

let rec overatoms f fm b =
  match fm with
    Atom(a) -> f a b
  | Not(p) -> overatoms f p b
  | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) ->
        overatoms f p (overatoms f q b)
  | Forall(x,p) | Exists(x,p) -> overatoms f p b
  | _ -> b;;

(* ------------------------------------------------------------------------- *)
(* Special case of a union of the results of a function over the atoms.      *)
(* ------------------------------------------------------------------------- *)

let atom_union f fm = setify (overatoms (fun h t -> f(h)@t) fm []);;

(* ------------------------------------------------------------------------- *)
(* General parsing of iterated infixes.                                      *)
(* ------------------------------------------------------------------------- *)

let rec parse_ginfix opsym opupdate sof subparser inp =
  let e1,inp1 = subparser inp in
  if inp1 <> [] & hd inp1 = opsym then
     parse_ginfix opsym opupdate (opupdate sof e1) subparser (tl inp1)
  else sof e1,inp1;;

let parse_left_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> opcon(f e1,e2)) (fun x -> x);;

let parse_right_infix opsym opcon =
  parse_ginfix opsym (fun f e1 e2 -> f(opcon(e1,e2))) (fun x -> x);;

let parse_list opsym =
  parse_ginfix opsym (fun f e1 e2 -> (f e1)@[e2]) (fun x -> [x]);;

(* ------------------------------------------------------------------------- *)
(* Other general parsing combinators.                                        *)
(* ------------------------------------------------------------------------- *)

let papply f (ast,rest) = (f ast,rest);;

let nextin inp tok = inp <> [] & hd inp = tok;;

let parse_bracketed subparser cbra inp =
  let ast,rest = subparser inp in
  if nextin rest cbra then ast,tl rest
  else failwith "Closing bracket expected";;

(* ------------------------------------------------------------------------- *)
(* Parsing of formulas, parametrized by atom parser "pfn".                   *)
(* ------------------------------------------------------------------------- *)

let rec parse_atomic_formula pfn vs inp =
  match inp with
    [] -> failwith "formula expected"
  | "false"::rest -> False,rest
  | "true"::rest -> True,rest
  | "("::rest -> (try pfn vs inp with Failure _ ->
                  parse_bracketed (parse_formula pfn vs) ")" rest)
  | "~"::rest -> papply (fun p -> Not p)
                        (parse_atomic_formula pfn vs rest)
  | "forall"::x::rest ->
        parse_quant pfn (x::vs) (fun (x,p) -> Forall(x,p)) x rest
  | "exists"::x::rest ->
        parse_quant pfn (x::vs) (fun (x,p) -> Exists(x,p)) x rest
  | _ -> pfn vs inp

and parse_quant pfn vs qcon x inp =
   match inp with
     [] -> failwith "Body of quantified term expected"
   | y::rest ->
        papply (fun fm -> qcon(x,fm))
               (if y = "." then parse_formula pfn vs rest
                else parse_quant pfn (y::vs) qcon y rest)

and parse_formula pfn vs inp =
   parse_right_infix "<->" (fun (p,q) -> Iff(p,q))
     (parse_right_infix "->" (fun (p,q) -> Imp(p,q))
         (parse_right_infix "\\/" (fun (p,q) -> Or(p,q))
             (parse_right_infix "/\\" (fun (p,q) -> And(p,q))
                  (parse_atomic_formula pfn vs)))) inp;;

(* ------------------------------------------------------------------------- *)
(* Printing of formulas, parametrized by atom printer.                       *)
(* ------------------------------------------------------------------------- *)

let rec strip_quant isforall fm =
  match (fm,isforall) with
    Forall(x,p),true -> papply (fun l -> x::l) (strip_quant isforall p)
  | Exists(x,p),false -> papply (fun l -> x::l) (strip_quant isforall p)
  | _ -> [],fm;;

let rec print_formula formater pfn prec fm =
  match fm with
    False -> pp_print_string formater "false"
  | True -> pp_print_string formater "true"
  | Atom(pargs) -> pfn prec pargs
  | Not(p) -> pp_print_string formater "~"; print_formula formater pfn 10 p
  | And(p,q) -> print_infix_formula formater pfn prec 8 "/\\" p q
  | Or(p,q) -> print_infix_formula formater pfn prec 6 "\\/" p q
  | Imp(p,q) -> print_infix_formula formater pfn prec 4 "->" p q
  | Iff(p,q) -> print_infix_formula formater pfn prec 2 "<->" p q
  | Forall(x,p) -> print_quant formater pfn prec "forall" (strip_quant true fm)
  | Exists(x,p) -> print_quant formater pfn prec "exists" (strip_quant false fm)

and print_quant formater pfn prec qname (bvs,bod) =
  if prec <> 0 then pp_print_string formater "(" else ();
  pp_print_string formater qname;
  do_list (fun v -> pp_print_string formater " "; pp_print_string formater v) bvs;
  pp_print_string formater ". "; open_box 0;
  print_formula formater pfn 0 bod;
  close_box();
  if prec <> 0 then pp_print_string formater ")" else ()

and print_infix_formula formater pfn oldprec newprec sym p q =
  if oldprec > newprec then (pp_print_string formater "("; open_box 0) else ();
  print_formula formater pfn (newprec+1) p;
  pp_print_string formater(" "^sym); pp_print_space formater ();
  print_formula formater pfn newprec q;
  if oldprec > newprec then (close_box(); pp_print_string formater ")") else ();;

let formula_printer formater pfn fm =
(*  open_box 0; pp_print_string formater "<<"; *)
  open_box 0; print_formula formater pfn 0 fm; close_box();
(*  pp_print_string formater ">>"; close_box() *);;
