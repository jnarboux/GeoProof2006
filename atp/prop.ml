(* ========================================================================= *)
(* Basic stuff for propositional logic: datatype, parsing and printing.      *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

open Num
open Lib
open Order
open Fol
open Formulas
open Format
open Intro
open Cooper

let negative = function (Not p) -> true | _ -> false;;

let positive lit = not(negative lit);;

let negate = function (Not p) -> p | p -> Not p;;
let rec specialize fm =
  match fm with
    Forall(x,p) -> specialize p
  | _ -> fm;;

let contradictory lits =
  let pos,neg = partition positive lits in
  intersect pos (map negate neg) <> [];;

let distrib s1 s2 = allpairs union s1 s2;;      (** Value restriction hell!  *)
                                                (** Need it for FOL formulas *)
let rec purednf fm =
  match fm with
    And(p,q) -> distrib (purednf p) (purednf q)
  | Or(p,q) -> union (purednf p) (purednf q)
  | _ -> [[fm]];;


let rec nnf fm =
  match fm with
    And(p,q) -> And(nnf p,nnf q)
  | Or(p,q) -> Or(nnf p,nnf q)
  | Imp(p,q) -> Or(nnf(Not p),nnf q)
  | Iff(p,q) -> Or(And(nnf p,nnf q),And(nnf(Not p),nnf(Not q)))
  | Not(Not p) -> nnf p
  | Not(And(p,q)) -> Or(nnf(Not p),nnf(Not q))
  | Not(Or(p,q)) -> And(nnf(Not p),nnf(Not q))
  | Not(Imp(p,q)) -> And(nnf p,nnf(Not q))
  | Not(Iff(p,q)) -> Or(And(nnf p,nnf(Not q)),And(nnf(Not p),nnf q))
  | Forall(x,p) -> Forall(x,nnf p)
  | Exists(x,p) -> Exists(x,nnf p)
  | Not(Forall(x,p)) -> Exists(x,nnf(Not p))
  | Not(Exists(x,p)) -> Forall(x,nnf(Not p))
  | _ -> fm;;

let mk_all x p = Forall(x,p) and mk_ex x p = Exists(x,p);;
let mk_and p q = And(p,q) and mk_or p q = Or(p,q);;

let rec pullquants fm =
  match fm with
    And(Forall(x,p),Forall(y,q)) -> pullquant_2 fm mk_all mk_and x y p q
  | Or(Exists(x,p),Exists(y,q)) -> pullquant_2 fm mk_ex mk_or x y p q
  | And(Forall(x,p),q) -> pullquant_l fm mk_all mk_and x p q
  | And(p,Forall(x,q)) -> pullquant_r fm mk_all mk_and x p q
  | Or(Forall(x,p),q) -> pullquant_l fm mk_all mk_or x p q
  | Or(p,Forall(x,q)) -> pullquant_r fm mk_all mk_or x p q
  | And(Exists(x,p),q) -> pullquant_l fm mk_ex mk_and x p q
  | And(p,Exists(x,q)) -> pullquant_r fm mk_ex mk_and x p q
  | Or(Exists(x,p),q) -> pullquant_l fm mk_ex mk_or x p q
  | Or(p,Exists(x,q)) -> pullquant_r fm mk_ex mk_or x p q
  | _ -> fm

and pullquant_l fm quant op x p q =
  let x' = variant x (fv fm) in
  quant x' (pullquants(op (formsubst (x := Var x') p) q))


and pullquant_r fm quant op x p q =
  let x' = variant x (fv fm) in
  quant x' (pullquants(op p (formsubst (x := Var x') q)))

and pullquant_2 fm quant op x y p q =
  let x' = variant x (fv fm) in
  quant x' (pullquants(op (formsubst (x := Var x') p)
                          (formsubst (y := Var x') q)));;


let rec prenex fm =
  match fm with
    Forall(x,p) -> Forall(x,prenex p)
  | Exists(x,p) -> Exists(x,prenex p)
  | And(p,q) -> pullquants(And(prenex p,prenex q))
  | Or(p,q) -> pullquants(Or(prenex p,prenex q))
  | _ -> fm;;

let psimplify1 fm =
  match fm with
    Not False -> True
  | Not True -> False
  | And(False,q) -> False
  | And(p,False) -> False
  | And(True,q) -> q
  | And(p,True) -> p
  | Or(False,q) -> q
  | Or(p,False) -> p
  | Or(True,q) -> True
  | Or(p,True) -> True
  | Imp(False,q) -> True
  | Imp(True,q) -> q
  | Imp(p,True) -> True
  | Imp(p,False) -> Not p
  | Iff(True,q) -> q
  | Iff(p,True) -> p
  | Iff(False,q) -> Not q
  | Iff(p,False) -> Not p
  | _ -> fm;;


let rec psimplify fm =
  match fm with
  | Not p -> psimplify1 (Not(psimplify p))
  | And(p,q) -> psimplify1 (And(psimplify p,psimplify q))
  | Or(p,q) -> psimplify1 (Or(psimplify p,psimplify q))
  | Imp(p,q) -> psimplify1 (Imp(psimplify p,psimplify q))
  | Iff(p,q) -> psimplify1 (Iff(psimplify p,psimplify q))
  | _ -> fm;;


let simplify1 fm =
  match fm with
    Forall(x,p) -> if mem x (fv p) then fm else p
  | Exists(x,p) -> if mem x (fv p) then fm else p
  | _ -> psimplify1 fm;;

let rec simplify fm =
  match fm with
    Not p -> simplify1 (Not(simplify p))
  | And(p,q) -> simplify1 (And(simplify p,simplify q))
  | Or(p,q) -> simplify1 (Or(simplify p,simplify q))
  | Imp(p,q) -> simplify1 (Imp(simplify p,simplify q))
  | Iff(p,q) -> simplify1 (Iff(simplify p,simplify q))
  | Forall(x,p) -> simplify1(Forall(x,simplify p))
  | Exists(x,p) -> simplify1(Exists(x,simplify p))
  | _ -> fm;;

let subsumes s1 s2 = psubset s2 s1;;

let subsume cls =
  filter (fun cl -> not(exists (subsumes cl) cls)) cls;;

let simpdnf fm =
  if fm = False then []
  else if fm = True then [[]]
  else subsume (filter (non contradictory) (purednf(nnf fm)));;
