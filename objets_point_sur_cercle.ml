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
open Options
open Optionsdeconfiguration
open Types_de_base
open Fonctions
open Repere
open Objets_graphiques
open Objets_point
open Printf
open Fol
open Formulas

class type t_objet_point_sur_cercle =
  object
    inherit Objets_point.t_objet_point
    method recalcule : unit
    method movable : unit -> bool
    method bouge : t_coords -> unit
    method depend_de : t_objet_graphique -> bool
    method def_langue_naturelle_corps : unit -> string
    method dependance : t_objet_graphique list

    method output_car_body : unit -> Xml.xml
    method output_kig_body : unit -> Xml.xml list
    method output_drg_body : unit -> Xml.xml
    method output_coq_body : unit -> Fol.fol Formulas.formula option
    method output_eukleides_body : unit -> string
    method output_pst_eucl_body : unit -> string
    method predicate_form : unit -> Fol.fol Formulas.formula option
  end

class objet_point_sur_cercle cercle c =
  object (self)
    inherit objet_point
       
    val mutable cost  = one /! two
    val mutable sint  = one /! two

    method movable () = true

    method def_langue_naturelle_corps () = 
        self#objet_nom^" be a point on "^cercle#objet_nom
									
    method recalcule =
      try
	let eq = cercle#equation_cercle in 
	  self#rend_calculable;
	  self#change_parametres
	    (COORDS_POINT (v_plus eq.center {x = cost *! eq.radius;
					     y = sint *! eq.radius}))
      with _ ->  self#rend_incalculable

    method bouge cl =
      try
	let eq = cercle#equation_cercle in
	let d = v_minus cl eq.center in	  
	let dist = hypothenuse d.x d.y in
	  cost <- d.x /! dist;
	  sint <- d.y /! dist;
	  self#recalcule
      with _ -> self#rend_incalculable
	
    method dependance = [cercle]

    method output_car_body () =
      Xml.Element("",[],[])

    method output_kig_body () =
      [Xml.Element("",[],[])]
    
    method output_drg_body () =
      let to_string x = to_string x !!precision in 
      Xml.Element("PointOnCircle",
		  [
		   ("name",self#objet_nom);
		   ("circle",cercle#objet_nom);
		   ("cost",to_string cost);
		   ("sint",to_string sint) (* TODO *)
		 ],[])

    method output_coq_body () =
       match !!coq_geom_language with
	   Narboux -> None
	 | Guilhot -> 
	     let a = Var(self#objet_nom) in
	     let o = Var(fst (unpack (cercle#defining_points ()))) in
	     let p = Var(snd (unpack (cercle#defining_points ()))) in
	       Some(Atom(R("cercle_rayon",[o;p;a])))

    method output_coq_tactic () = 
      match !!coq_geom_language with 
	  Narboux -> 
	    let o,i = unpack (cercle#defining_points ()) in
	      Some (sprintf "point_on_line %s %s %s %s" self#objet_nom o i "H")
	| Guilhot -> None

    method output_pst_eucl_body () = ""
	
    method output_eukleides_body () = 
      let angle = 
	let t = arccos cost in
	to_string (to_deg t) !!precision in      
	sprintf "%s = point(%s,%s:)" self#objet_nom cercle#objet_nom angle

    method predicate_form () =
      let a = Var(self#objet_nom) in
      let o = Var(fst (unpack (cercle#defining_points ()))) in
      let p = Var(snd (unpack (cercle#defining_points ()))) in
	Some(Atom(R("lengths_eq",[o;p;o;a])))
	  
    initializer
      self#bouge c
  end
