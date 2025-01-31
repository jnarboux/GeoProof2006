(* ========================================================================= *)
(* Grobner basis algorithm.                                                  *)
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
open Prop

(* ------------------------------------------------------------------------- *)
(* Monomial ordering.                                                        *)
(* ------------------------------------------------------------------------- *)

let morder_lt m1 m2 =
  let n1 = itlist (+) m1 0 and n2 = itlist (+) m2 0 in
  n1 < n2 or n1 = n2 & lexord(>) (rev m1) (rev m2);;

(* ------------------------------------------------------------------------- *)
(* Arithmetic on canonical polynomials.                                      *)
(* ------------------------------------------------------------------------- *)

let rec grob_add l1 l2 =
  match (l1,l2) with
    ([],l2) -> l2
  | (l1,[]) -> l1
  | ((c1,m1)::o1,(c2,m2)::o2) ->
        if m1 = m2 then
          let c = c1+/c2 and rest = grob_add o1 o2 in
          if c =/ Int 0 then rest else (c,m1)::rest
        else if morder_lt m2 m1 then (c1,m1)::(grob_add o1 l2)
        else (c2,m2)::(grob_add l1 o2);;

let grob_mmul (c1,m1) (c2,m2) = (c1*/c2,map2 (+) m1 m2);;

let rec grob_cmul cm pol = map (grob_mmul cm) pol;;

let grob_neg = map (fun (c,m) -> (minus_num c,m));;

let grob_sub l1 l2 = grob_add l1 (grob_neg l2);;

let rec grob_mul l1 l2 =
  match l1 with
    [] -> []
  | (h1::t1) -> grob_add (grob_cmul h1 l2) (grob_mul t1 l2);;

(* ------------------------------------------------------------------------- *)
(* Monomial division operation.                                              *)
(* ------------------------------------------------------------------------- *)

let mdiv =
  let index_sub n1 n2 = if n1 < n2 then failwith "mdiv" else n1-n2 in
  fun (c1,m1) (c2,m2) -> (c1//c2,map2 index_sub m1 m2);;

(* ------------------------------------------------------------------------- *)
(* Reduce monomial cm by polynomial pol, returning replacement for cm.       *)
(* ------------------------------------------------------------------------- *)

let reduce1 cm pol =
  match pol with
    [] -> failwith "reduce1"
  | hm::cms -> let (c,m) = mdiv cm hm in grob_cmul (minus_num c,m) cms;;

(* ------------------------------------------------------------------------- *)
(* Try this for all polynomials in a basis.                                  *)
(* ------------------------------------------------------------------------- *)

let reduceb cm basis = tryfind (reduce1 cm) basis;;

(* ------------------------------------------------------------------------- *)
(* Reduction of a polynomial (always picking largest monomial possible).     *)
(* ------------------------------------------------------------------------- *)

let rec reduce basis pol =
  match pol with
    [] -> []
  | cm::ptl -> try reduce basis (grob_add (reduceb cm basis) ptl)
               with Failure _ -> cm::(reduce basis ptl);;

(* ------------------------------------------------------------------------- *)
(* Lowest common multiple of two monomials.                                  *)
(* ------------------------------------------------------------------------- *)

let mlcm (c1,m1) (c2,m2) = (Int 1,map2 max m1 m2);;

(* ------------------------------------------------------------------------- *)
(* Compute S-polynomial of two polynomials (zero for the orthogonal case).   *)
(* ------------------------------------------------------------------------- *)

let spoly pol1 pol2 =
  match (pol1,pol2) with
    ([],p) -> []
  | (p,[]) -> []
  | (m1::ptl1,m2::ptl2) ->
        let m = mlcm m1 m2 in
        if snd(m) = snd(grob_mmul  m1 m2) then []
        else grob_sub (grob_cmul (mdiv m m1) ptl1)
                      (grob_cmul (mdiv m m2) ptl2);;

(* ------------------------------------------------------------------------- *)
(* Grobner basis algorithm.                                                  *)
(* ------------------------------------------------------------------------- *)

let rec grobner basis pairs =
  print_string(string_of_int(length basis)^" basis elements and "^
               string_of_int(length pairs)^" pairs");
  print_newline();
  match pairs with
    [] -> basis
  | (p1,p2)::opairs ->
        let sp = reduce basis (spoly p1 p2) in
        if sp = [] then grobner basis opairs
        else if forall (forall ((=) 0) ** snd) sp then [sp] else
        let newcps = map (fun p -> p,sp) basis in
        grobner (sp::basis) (opairs @ newcps);;

(* ------------------------------------------------------------------------- *)
(* Overall function.                                                         *)
(* ------------------------------------------------------------------------- *)

let groebner basis = grobner basis (distinctpairs basis);;

(* ------------------------------------------------------------------------- *)
(* Convert formula into canonical form.                                      *)
(* ------------------------------------------------------------------------- *)

let grob_var vars x =
  [Int 1,map (fun y -> if y = x then 1 else 0) vars]

let grob_const vars n =
  if n =/ Int 0 then [] else [n,map (fun k -> 0) vars];;

let rec grobterm vars tm =
  match tm with
    Var x -> grob_var vars x
  | Fn("-",[t]) -> grob_neg (grobterm vars t)
  | Fn("+",[s;t]) ->
        grob_add (grobterm vars s) (grobterm vars t)
  | Fn("-",[s;t]) ->
        grob_sub (grobterm vars s) (grobterm vars t)
  | Fn("*",[s;t]) ->
        grob_mul (grobterm vars s) (grobterm vars t)
  | Fn("^",[t;n]) ->
        funpow (int_of_num(dest_numeral n))
               (grob_mul (grobterm vars t)) (grob_const vars (Int 1))
  | _ -> grob_const vars (dest_numeral tm);;

let grobatom vars fm =
  match fm with
      Atom(R("=",[s;t])) -> grobterm vars (Fn("-",[s;t]))
    | True ->  grobterm vars (Fn("-",[Var "__";Var "__"]))
    | False -> grobterm vars (Fn("-",[Var "__";Var "_1"]))
    | _ -> failwith "grobatom: not an equation";;

(* ------------------------------------------------------------------------- *)
(* Use the Rabinowitsch trick to eliminate inequations.                      *)
(* That is, replace p =/= 0 by exists w. p * w = 1.                          *)
(* ------------------------------------------------------------------------- *)

let rabinowitsch vars v p =
   grob_sub (grob_const vars (Int 1)) (grob_mul (grob_var vars v) p);;

(* ------------------------------------------------------------------------- *)
(* Universal complex number decision procedure based on Grobner bases.       *)
(* ------------------------------------------------------------------------- *)

let grobner_trivial fms =
  let vars0 = itlist (union ** fv) fms []
  and eqs,neqs = partition positive fms in
  let rvs = map (fun n -> variant ("_"^string_of_int n) vars0)
                (1--length neqs) in
  let vars = vars0 @ rvs in
  let poleqs = map (grobatom vars) eqs
  and polneqs = map (grobatom vars ** negate) neqs in
  let pols = poleqs @ map2 (rabinowitsch vars) rvs polneqs in
  reduce (groebner pols) (grob_const vars (Int 1)) = [];;

let grobner_decide fm =
  let fm1 = specialize(prenex(nnf(simplify fm))) in
  forall grobner_trivial (simpdnf(nnf(Not fm1)));;


