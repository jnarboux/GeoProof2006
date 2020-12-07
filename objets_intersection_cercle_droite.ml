(*******************************************************************************)
(* GeoProof, an interactive geometry tool writen in OCaml.                    *)
(* Copyright (C) 2004 Nicolas François et Julien Narboux                       *)
(*                                                                             *)
(* This program is free software; you can redistribute it and/or               *)
(* modify it under the terms of the GNU General Public License                 *)
(* as published by the Free Software Foundation; either version 2              *)
(* of the License, or (at your option) any later version.                      *)
(*                                                                             *)
(* This program is distributed in the hope that it will be useful,             *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of              *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               *)
(* GNU General Public License for more details.                                *)
(*                                                                             *)
(* You should have received a copy of the GNU General Public License           *)
(* along with this program; if not, write to the Free Software                 *)
(* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. *)
(*******************************************************************************)

open Geometric_functions.GeometryFunctions
open Types_de_base
open Objets_graphiques
open Objets_point
open Fonctions
open Repere
open Printf
open Fol
open Formulas
open Options
open Optionsdeconfiguration

class type t_objet_intersection_cercle_droite =
  object
    inherit Objets_point.t_objet_point
    method recalcule : unit
    method depend_de : t_objet_graphique -> bool
    method dependance : t_objet_graphique list
    method def_langue_naturelle_corps : unit -> string

    method output_car_body : unit -> Xml.xml
    method output_kig_body : unit -> Xml.xml list
    method output_drg_body : unit -> Xml.xml
    method output_coq_body : unit -> Fol.fol Formulas.formula option
    method output_eukleides_body : unit -> string
    method output_pst_eucl_body : unit -> string
    method predicate_form : unit -> Fol.fol Formulas.formula option
  end
      
      
class objet_intersection_cercle_droite c1 d2 epsilon =
  object (self)
    inherit objet_point

    method def_langue_naturelle_corps () = 
      self#objet_nom^" be the intersection of "^c1#objet_nom^" and "^d2#objet_nom

    method recalcule =
      try
	let eq1 = c1#equation_cercle
	and eq2 = d2#equation_droite in
	let ah = dot_product (v_minus eq1.center eq2.orig) (eq2.dir)
	in
	let ch2 =  dot_product (v_minus eq1.center eq2.orig) (v_minus eq1.center eq2.orig) 
	  -! ah *! ah
	in
	let delta = sqrt (eq1.radius *! eq1.radius -! ch2) in
	let eps = of_int epsilon in
	self#rend_calculable;
	self#change_parametres
	  (COORDS_POINT 
	     (v_plus eq2.orig (v_scalar  (ah +! eps *! delta) eq2.dir)) 
	  )
      with _ -> self#rend_incalculable
      	  
    method dependance = [c1; d2]

    method output_car_body () =
      Xml.Element("",[],[])
	
    method output_kig_body () =
      [Xml.Element("",[],[])]
	
    method output_drg_body () =
    let name = 
	match epsilon with (* TODO change epsion into an enumarated type *)
	  1 -> "IntersectionCircleCircle1"
	| -1 -> "IntersectionCircleCircle2"
	| _ -> assert false
      in  
    Xml.Element(name,[
		("name",self#objet_nom); 
		("circle",c1#objet_nom);
		("line",d2#objet_nom)
	      ],[])

    method output_coq_body () = 
    match !!coq_geom_language with 
	Narboux -> None	
      | Guilhot -> 
	  let p =  Var(self#objet_nom) in
	  let c1 = unpack (c1#defining_points ()) in
	  let d2 = unpack (d2#defining_points ()) in
	  let a = Var(fst d2) in
	  let b = Var(snd d2) in
	  let o = Var(fst c1) in
	  let i = Var(snd c1) in
	  let pred = And(Atom(R("cercle_rayon",[o;i;p])),Atom(R("alignes",[a;b;p]))) in
	    match epsilon with
		1 -> Some(pred)
	      | -1 -> Some(And(pred,Not(Atom(R("=",[p;Var(self#second_intersection())])))))
	      | _ -> assert false
		  
    method output_pst_eucl_body () = ""
	
    method private second_intersection () =
    (* Ugly hack here :
	 We assume that both intersections are always defined 
	 and we look for the name of the second point.
       *)
      let l = Construction.filter 
	(fun o -> o#dependance = self#dependance && o#objet_nom <> self#objet_nom)
      in
	match l with
	  | [] -> assert false
	  | x::_ -> x#objet_nom

    method output_eukleides_body () = 
      match epsilon with
	  1 -> sprintf "%s %s intersection(%s,%s)" 
	    self#objet_nom 
	    (self#second_intersection ())
	    d2#objet_nom c1#objet_nom
	| -1 -> ""
	| _ -> assert false    

   method predicate_form () = 
     let p =  Var(self#objet_nom) in
     let c1 = unpack (c1#defining_points ()) in
     let d2 = unpack (d2#defining_points ()) in
     let a = Var(fst d2) in
     let b = Var(snd d2) in
     let o = Var(fst c1) in
     let i = Var(snd c1) in	
       Some(And(And(Atom(R("collinear",[p;a;b])),
		    Atom(R("lengths_eq",[o;p;o;i]))),
		    Not(Atom(R("isotropic",[a;b]))))
	   )
	
    initializer self#recalcule
  end


