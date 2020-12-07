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

class type t_objet_intersection_droite_droite =
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


class objet_intersection_droite_droite d1 d2 =
  object (self)
    inherit objet_point as obp

    method def_langue_naturelle_corps () = 
      self#objet_nom^" be the intersection of "^d1#objet_nom^" and "^d2#objet_nom

    method recalcule =
      try
	let eq1 = d1#equation_droite
	and eq2 = d2#equation_droite in 
	self#rend_calculable;
	self#change_parametres
	  (COORDS_POINT 
	     (intersection_line_line eq1 eq2))  
      with _ -> self#rend_incalculable
	  
    method dependance = [d1; d2]
	
    method output_car_body () =
      Xml.Element("",[],[])

    method output_kig_body () =
      [Xml.Element("",[],[])]
    
    method output_drg_body () =
      Xml.Element("IntersectionLineLine",
		  [
		   ("name",self#objet_nom); 
		   ("first",d1#objet_nom);
		   ("second",d2#objet_nom)
		 ],[])

    method output_coq_body () = 
      let p =  Var(self#objet_nom) in
      let d1 = unpack (d1#defining_points ()) in
      let d2 = unpack (d2#defining_points ()) in
      let a = Var(fst d1) in
      let b = Var(snd d1) in
      let c = Var(fst d2) in
      let d = Var(snd d2) in
	match !!coq_geom_language with
	    Narboux ->
	      Some(Atom(R("inter_ll",[p;a;b;c;d])))
	  | Guilhot ->
	      Some(Atom(R("=",[p;Fn("pt_intersection",[Fn("droite",[a;b]);Fn("droite",[c;d])])])))

    method output_coq_tactic () =
      let a,b = unpack (d1#defining_points ()) in
      let c,d = unpack (d2#defining_points ()) in
	match !!coq_geom_language with
	    Narboux ->
	      Some (sprintf "point_on_intersection_lines %s %s %s %s %s" 
		      self#objet_nom a b c d)	      
	  | Guilhot ->
	      Some (sprintf "soit_intersection %s %s %s %s %s" 
		      self#objet_nom a b c d)	      

    method output_pst_eucl_body () = ""
	
    method output_eukleides_body () = 
      let aux x = 
	if x#objet_type = TYPE_SEGMENT then
	  sprintf "line(%s,%s)" 
	    (List.hd x#dependance)#objet_nom (* TODO clean this *)   
	    (List.hd (List.tl x#dependance))#objet_nom
	else
	  x#objet_nom
      in
      let d1 = aux d1 in
      let d2 = aux d2 in
	sprintf "%s = intersection(%s,%s)" self#objet_nom d1 d2

    method predicate_form () =
      try
      let p =  Var(self#objet_nom) in
      let d1 = unpack (d1#defining_points ()) in
      let d2 = unpack (d2#defining_points ()) in
      let a = Var(fst d1) in
      let b = Var(snd d1) in
      let c = Var(fst d2) in
      let d = Var(snd d2) in	
	Some(And(And(
		   Atom(R("collinear",[p;a;b])),
		   Atom(R("collinear",[p;c;d]))),
		 Not(Atom(R("parallel",[a;b;c;d])))))
      with _ -> failwith "ici"
 
    initializer 
      self#recalcule
  end


