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
open Fonctions
open Fol
open Formulas
open Options
open Optionsdeconfiguration


class type t_objet_intersection_cercles =
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


class objet_intersection_cercles c1 c2 epsilon =
  object (self)
    inherit objet_point
	
    method def_langue_naturelle_corps () = 
      self#objet_nom^" be the intersection of "^c1#objet_nom^" and "^c2#objet_nom
								
    method recalcule =
      try
	let eq1 = c1#equation_cercle
	and eq2 = c2#equation_cercle in
	let c1c2 = (v_minus eq2.center eq1.center) in

	let radius12 = eq1.radius *! eq1.radius and radius22 = eq2.radius *! eq2.radius in
	let c1c22 = dot_product c1c2 c1c2 in
	let lambda =  (one +! (radius12 -! radius22) /! c1c22) /! two in
	let mu = sqrt ((radius12 -! lambda *! lambda *! c1c22) /! c1c22) in
	let eps = of_int epsilon in
	self#rend_calculable;
	self#change_parametres
	  (COORDS_POINT 
	     (v_plus (v_plus eq1.center (v_scalar lambda c1c2)) (v_scalar (eps *! mu) {x=c1c2.y;y=neg c1c2.x})))

      with _ -> self#rend_incalculable
	  
    method dependance = [c1; c2]
     
    method output_car_body () =
      Xml.Element("",[],[])
	
    method output_kig_body () =
      [Xml.Element("",[],[])]
	
    method output_drg_body () =
      let name = 
	match epsilon with
	  1 -> "IntersectionCircleCircle1"
	| -1 -> "IntersectionCircleCircle2"
	| _ -> assert false
      in
      Xml.Element(name,[
		  ("name",self#objet_nom); 
		  ("first",c1#objet_nom);
		  ("second",c2#objet_nom)
		],[])

    method output_coq_body () = 
      match !!coq_geom_language with 
	  Narboux -> None	
	| Guilhot -> 
	    let p =  Var(self#objet_nom) in
	    let c1 = unpack (c1#defining_points ()) in
	    let c2 = unpack (c2#defining_points ()) in
	    let o1 = Var(fst c1) in
	    let i1 = Var(snd c1) in
	    let o2 = Var(fst c2) in
	    let i2 = Var(snd c2) in	
	      Some(And(Atom(R("cercle_rayon",[o1;i1;p])),Atom(R("cercle_rayon",[o2;i2;p]))))

    method output_pst_eucl_body () = ""

    method output_eukleides_body () = 
      (* Ugly hack here :
	 We assume that both intersections are always defined 
	 and we look for the name of the second point.
       *)
      
      let second_intersection =
	let l = Construction.filter 
	    (fun o -> o#dependance = self#dependance && o#objet_nom <> self#objet_nom)
	in
	match l with
	| [] -> assert false
	| x::_ -> x#objet_nom
      in
      
      match epsilon with
	1 -> sprintf "%s %s intersection(%s,%s)" self#objet_nom second_intersection c1#objet_nom c2#objet_nom
      | -1 -> ""
      | _ -> assert false

    method predicate_form () = 
      let p =  Var(self#objet_nom) in
      let c1 = unpack (c1#defining_points ()) in
      let c2 = unpack (c2#defining_points ()) in
      let o1 = Var(fst c1) in
      let i1 = Var(snd c1) in
      let o2 = Var(fst c2) in
      let i2 = Var(snd c2) in	
	Some(And(And(Atom(R("lengths_eq",[o1;i1;o1;p])),
		     Atom(R("lengths_eq",[o2;i2;o2;p]))),
		 Not(Atom(R("isotropic",[o1;o2])))))

	    
    initializer self#recalcule
  end
    

