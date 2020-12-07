(* ========================================================================= *)
(* Geometry theorem proving.                                                 *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* List of geometric properties with their coordinate translations.          *)
(* ------------------------------------------------------------------------- *)

open Fol
open Lib
open Formulas
open Cooper
open Num

(*
   "angles_eq",
   "((2_y - 1_y) * (2_x - 3_x) - (2_y - 3_y) * (2_x - 1_x)) *
     ((5_x - 4_x) * (5_x - 6_x) + (5_y - 4_y) * (5_y - 6_y)) =
     ((5_y - 4_y) * (5_x - 6_x) - (5_y - 6_y) * (5_x - 4_x)) *
     ((2_x - 1_x) * (2_x - 3_x) + (2_y - 1_y) * (2_y - 3_y))"];;
*)

let square t = Fn("*",[t;t])

let dist a b = 
  Fn("+",[square (Fn("-",[Var(a^"_x");Var(b^"_x")]));square (Fn("-",[Var(a^"_y");Var(b^"_y")]))])

let coordinations =
 [
 "is_midpoint",(3,And(Atom(R("=",[Fn("+",[Var("1_x");Var("1_x")]);Fn("+",[Var("2_x");Var("3_x")])])),
                    Atom(R("=",[Fn("+",[Var("1_y");Var("1_y")]);Fn("+",[Var("2_y");Var("3_y")])]))));
 "parallel", (4,Atom(R("=",[Fn("*",[Fn("-",[Var("1_x");Var("2_x")]);Fn("-",[Var("3_y");Var("4_y")])]);Fn("*",[Fn("-",[Var("1_y");Var("2_y")]);Fn("-",[Var("3_x");Var("4_x")])])])));
 "perpendicular",(4,Atom(R("=",
			[
			 Fn("+",[Fn("*",[Fn("-",[Var("1_x");Var("2_x")]);Fn("-",[Var("3_x");Var("4_x")])]);
				 Fn("*",[Fn("-",[Var("1_y");Var("2_y")]);Fn("-",[Var("3_y");Var("4_y")])])]);
			  Fn("-",[Var("1_x");Var("1_x")])
		       ])));
  "collinear",(3,Atom(R("=",[Fn("*",[Fn("-",[Var("1_x");Var("2_x")]);Fn("-",[Var("2_y");Var("3_y")])]);Fn("*",[Fn("-",[Var("1_y");Var("2_y")]);Fn("-",[Var("2_x");Var("3_x")])])])));
  "lengths_eq",(4,Atom(R("=",[dist "1" "2"; dist "3" "4"])));
  "is_intersection",(5,And(Atom(R("=",[Fn("*",[Fn("-",[Var("1_x");Var("2_x")]);Fn("-",[Var("2_y");Var("3_y")])]);
				    Fn("*",[Fn("-",[Var("1_y");Var("2_y")]);Fn("-",[Var("2_x");Var("3_x")])])])),
			Atom(R("=",[Fn("*",[Fn("-",[Var("1_x");Var("4_x")]);Fn("-",[Var("4_y");Var("5_y")])]);
				    Fn("*",[Fn("-",[Var("1_y");Var("4_y")]);Fn("-",[Var("4_x");Var("5_x")])])]))));
  "=",(2,And(Atom(R("=",[Var("1_x");Var("2_x")])),
	  Atom(R("=",[Var("1_y");Var("2_y")]))));
  "angles_eq",(6,Atom(R("=",[Fn("*",[Fn("-",[
				     Fn("*",[Fn("-",[Var("2_y");Var("1_y")]);Fn("-",[Var("2_x");Var("3_x")])]);
				     Fn("*",[Fn("-",[Var("2_y");Var("3_y")]);Fn("-",[Var("2_x");Var("1_x")])])]);
				  Fn("+",[
				     Fn("*",[Fn("-",[Var("5_x");Var("4_x")]);Fn("-",[Var("5_x");Var("6_x")])]);
				     Fn("*",[Fn("-",[Var("5_y");Var("4_y")]);Fn("-",[Var("5_y");Var("6_y")])])])]);
			  Fn("*",[Fn("-",[
				     Fn("*",[Fn("-",[Var("5_y");Var("4_y")]);Fn("-",[Var("5_x");Var("6_x")])]);
				     Fn("*",[Fn("-",[Var("5_y");Var("6_y")]);Fn("-",[Var("5_x");Var("4_x")])])]);
				  Fn("+",[
				     Fn("*",[Fn("-",[Var("2_x");Var("1_x")]);Fn("-",[Var("2_x");Var("3_x")])]);
				     Fn("*",[Fn("-",[Var("2_y");Var("1_y")]);Fn("-",[Var("2_y");Var("3_y")])])])])]))) 
]

(* ------------------------------------------------------------------------- *)
(* Convert formula into coordinate form.                                     *)
(* ------------------------------------------------------------------------- *)

let inst_coord fms pat =
  let xtms,ytms = unzip
    (map (fun (Var v) -> Var(v^"_x"),Var(v^"_y")) fms) in
  let xs = map (fun n -> string_of_int n^"_x") (1--length fms)
  and ys = map (fun n -> string_of_int n^"_y") (1--length fms) in
  formsubst (instantiate (xs @ ys) (xtms @ ytms)) pat;;

let coordinate fm = onatoms
  (fun (R(a,args)) -> 
     let p = assoc a coordinations in
       (* if there is not the right number of element we raise an exception *)
       if fst p <> List.length args then raise Not_found;
       let formula = snd p in 
	 inst_coord args formula) fm;;

(* ------------------------------------------------------------------------- *)
(* Choose one point to be the origin and rotate to zero another x coordinate *)
(* ------------------------------------------------------------------------- *)

let originate fm =
  let a::b::ovs as vars = fv fm in
  let rfn = itlist (fun v -> v |-> Fn("0",[]))
                   [a^"_x"; a^"_y"; b^"_y"] undefined in
  formsubst rfn (coordinate fm);;

(* ------------------------------------------------------------------------- *)
(* Reduce p using triangular set, collecting degenerate conditions.          *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Auxiliary polynomial fonctions                                            *)
(* ------------------------------------------------------------------------- *)

let rec poly_add vars pol1 pol2 =
  match (pol1,pol2) with
   (Fn("+",[c; Fn("*",[Var x; p])]),Fn("+",[d; Fn("*",[Var y; q])])) ->
        if earlier vars x y then poly_cadd vars pol2 pol1
        else if earlier vars y x then poly_cadd vars pol1 pol2 else
        let cd = poly_add vars c d and pq = poly_add vars p q in
        if pq = Fn("0",[]) then cd
        else Fn("+",[cd; Fn("*",[Var x; pq])])
    | (_,Fn("+",_)) -> poly_cadd vars pol1 pol2
    | (Fn("+",_),pol2) -> poly_cadd vars pol2 pol1
    | _ -> numeral2 (+/) pol1 pol2
and poly_cadd vars =
  fun pol1 (Fn("+",[d; Fn("*",[Var y; q])])) ->
        Fn("+",[poly_add vars pol1 d; Fn("*",[Var y; q])]);;


let rec poly_neg =
  function (Fn("+",[c; Fn("*",[Var x; p])])) ->
                Fn("+",[poly_neg c; Fn("*",[Var x; poly_neg p])])
         | n -> numeral1 minus_num n;;

let poly_sub vars p q = poly_add vars p (poly_neg q);;

let rec poly_mul vars pol1 pol2 =
  match (pol1,pol2) with
   (Fn("+",[c; Fn("*",[Var x; p])]),Fn("+",[d; Fn("*",[Var y; q])])) ->
        if earlier vars x y then poly_cmul vars pol2 pol1
        else poly_cmul vars pol1 pol2
  | (Fn("0",[]),_) -> Fn("0",[])
  | (_,Fn("0",[])) -> Fn("0",[])
  | (_,Fn("+",_)) -> poly_cmul vars pol1 pol2
  | (Fn("+",_),_) -> poly_cmul vars pol2 pol1
  | _ -> numeral2 ( */ ) pol1 pol2
and poly_cmul vars =
  fun pol1 (Fn("+",[d; Fn("*",[Var y; q])])) ->
        poly_add vars (poly_mul vars pol1 d)
                     (Fn("+",[Fn("0",[]);
                              Fn("*",[Var y; poly_mul vars pol1 q])]))

let poly_pow vars p n = funpow n (poly_mul vars p) (Fn("1",[]))


let rec degree vars =
  function (Fn("+",[c; Fn("*",[Var x; p])])) when x = hd vars ->
                1 + degree vars p
         | _ -> 0

let rec coefficients vars =
  function (Fn("+",[c; Fn("*",[Var x; q])]) as p) when x = hd vars ->
                c::(coefficients vars q)
         | p -> [p]

let head vars p = last(coefficients vars p)

let is_constant vars p =
  match p with
    Fn("+",[c; Fn("*",[Var x; q])]) when x = hd vars -> false
  | _ -> true;;

let rec behead vars =
  function Fn("+",[c; Fn("*",[Var x; p])]) when x = hd vars ->
                let p' = behead vars p in
                if p' = Fn("0",[]) then c
                else Fn("+",[c; Fn("*",[Var x; p'])])
         | _ -> Fn("0",[]);;

let dest_eq =
  function (Atom(R("=",[s;t]))) -> s,t
         | _ -> failwith "dest_eq: not an equation";;

let pdivide =
  let shift1 x p = Fn("+",[Fn("0",[]); Fn("*",[Var x; p])]) in
  let rec pdivide_aux vars a n p k s =
    if s = Fn("0",[]) then (k,s) else
    let b = head vars s and m = degree vars s in
    if m < n then (k,s) else
    let p' = funpow (m - n) (shift1 (hd vars)) p in
    if a = b then
      pdivide_aux vars a n p k (poly_sub vars s p')
    else
      pdivide_aux vars a n p (k+1)
        (poly_sub vars (poly_mul vars a s) (poly_mul vars b p')) in
  fun vars s p -> pdivide_aux vars (head vars p) (degree vars p) p 0 s

let rec polynate vars tm =
  match tm with
    Var x -> Fn("+",[Fn("0",[]); Fn("*",[Var x; Fn("1",[])])])
  | Fn(ns,[]) -> if can num_of_string ns then tm
                 else failwith "Unexpected constant"
  | Fn("^",[p;Fn(n,[])]) ->
       poly_pow vars (polynate vars p) (int_of_string n)
  | Fn("*",[s;t]) -> poly_mul vars (polynate vars s) (polynate vars t)
  | Fn("+",[s;t]) -> poly_add vars (polynate vars s) (polynate vars t)
  | Fn("-",[s;t]) -> poly_sub vars (polynate vars s) (polynate vars t)
  | Fn("-",[t]) -> poly_neg (polynate vars t)
  | Fn(s,_) -> failwith ("Unexpected function symbol: "^s);;


let polyatom vars fm =
  match fm with
      Atom(R(a,[s;t])) ->
        Atom(R(a,[polynate vars (Fn("-",[s;t])); Fn("0",[])]))
    | True ->  Atom(R("=",[polynate vars (Fn("0",[])); Fn("0",[])]))
    | False ->  Atom(R("=",[polynate vars (Fn("1",[])); Fn("0",[])]))
    | _ -> failwith "polyatom: not an atom";;

let rec pprove vars triang p degens =
  if p = Fn("0",[]) then degens else
  match triang with
    [] -> Atom(R("=",[p;Fn("0",[])]))::degens
  | (Fn("+",[c;Fn("*",[Var x;_])]) as q)::qs ->
        if x <> hd vars then
          if mem (hd vars) (fvt p)
          then itlist (pprove vars triang) (coefficients vars p) degens
          else pprove (tl vars) triang p degens
        else
          let k,p' = pdivide vars p q in
          if k = 0 then pprove vars qs p' degens else
          let degens' =
            Not(Atom(R("=",[head vars q; Fn("0",[])])))::degens in
          if is_constant vars p' then pprove vars qs p' degens' else
          itlist (pprove vars qs) (coefficients vars p') degens'
  | (q::qs) -> Not(Or(False,Atom(R("=",[q; Fn("0",[])]))))::degens;;

(* ------------------------------------------------------------------------- *)
(* Triangulate a set of polynomials.                                         *)
(* ------------------------------------------------------------------------- *)

let rec triangulate vars consts pols =
  if vars = [] then pols
  else if pols = [] then triangulate (tl vars) [] consts else
  let cns,tpols = partition (is_constant vars) pols in
  if cns <> [] then triangulate vars (cns @ consts) tpols else
  if length pols = 1 then pols @ triangulate (tl vars) [] consts else
  let n = end_itlist min (map (degree vars) pols) in
  let p = find (fun p -> degree vars p = n) pols in
  let ps = subtract pols [p] in
  if n = 1 then
    p :: (triangulate (tl vars) []
            (consts @ map (fun q -> snd(pdivide vars q p)) ps))
  else
    let m = end_itlist min (map (degree vars) ps) in
    let q = find (fun q -> degree vars q = m) ps in
    let qs = subtract ps [q] in
    let rs = p::(snd(pdivide vars q p))::qs in
    triangulate vars consts rs;;

(* ------------------------------------------------------------------------- *)
(* Auxiliary stuff.                                                          *)
(* ------------------------------------------------------------------------- *)

let dest_imp fm =
  match fm with
    Imp(p,q) -> p,q
  | _ -> failwith "dest_imp";;

let lhs eq = fst(dest_eq eq) and rhs eq = snd(dest_eq eq);;

let rec disjuncts fm =
  match fm with
    Or(p,q) -> disjuncts p @ disjuncts q
  | _ -> [fm];;

let rec conjuncts fm =
  match fm with
    And(p,q) -> conjuncts p @ conjuncts q
  | _ -> [fm];;

(* ------------------------------------------------------------------------- *)
(* Trivial version of Wu's method based on repeated pseudo-division.         *)
(* ------------------------------------------------------------------------- *)

let wu fm vars zeros =
  (* let gfm0 = coordinate fm in *)
  let gfm = formsubst
    (itlist (fun v -> v |-> Fn("0",[])) zeros undefined) fm in
  if not (set_eq vars (fv gfm))
  then failwith "wu: wrong variable set" else
  let ant,con = dest_imp gfm in
  let pols = map (lhs ** polyatom vars) (conjuncts ant)
  and ps = map (lhs ** polyatom vars) (conjuncts con) in
  let tri = triangulate vars [] pols in
  itlist (fun p -> union(pprove vars tri p [])) ps [];;

let vars fm = 
  let rec aux l = match l with      
    | a::r -> 
	begin
	  match a with 
	      Var(x) -> [x]@(aux r) 
	    | Fn(_,tl) -> (aux tl)@(aux r)
	end
    | [] -> []
  in
  let f a = 
    match a with
	R(_,l) -> aux l 
      |_ -> [] 
  in
    setify (Formulas.overatoms (fun a b -> (f a)@b) fm [])  
      
let wubis fm = 
(*MG
  Debug.pdb "Variables :";
  List.iter Debug.pdb (vars fm);
*)
  let l = wu fm (vars fm) [] in
    List.iter (fun x -> Format.print_string "\n";printer x) l;
  let l = 
    let is_trivial a = 
      match a with 
	  Not(Atom(R("=",[Fn(a,[]);Fn(b,[])]))) when a<>b -> false
	| _ -> true
    in
      List.filter is_trivial l in
    List.iter (fun x -> Format.print_string "\n";printer x) l;
    l=[]  
