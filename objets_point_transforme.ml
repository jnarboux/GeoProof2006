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
open Options
open Optionsdeconfiguration
open Fol
open Formulas

class type t_objet_point_transforme =
  object 
    inherit Objets_point.t_objet_point
	
    method depend_de : t_objet_graphique -> bool
    method def_langue_naturelle_corps : unit -> string
    method dependance : t_objet_graphique list
    method recalcule : unit

    method output_car_body : unit -> Xml.xml
    method output_kig_body : unit -> Xml.xml list
    method output_drg_body : unit -> Xml.xml
    method output_coq_body : unit -> Fol.fol Formulas.formula option
    method output_eukleides_body : unit -> string
    method output_pst_eucl_body : unit -> string
    method predicate_form : unit -> Fol.fol Formulas.formula option
  end
      
class objet_point_transforme obj1 obj2 =
  object (self)
    inherit objet_point

    method def_langue_naturelle_corps () =
      match obj2#objet_type with
	TYPE_POINT | TYPE_DROITE | TYPE_SEGMENT ->
	  self#objet_nom^" is the symetric of "^obj1#objet_nom^" using "^obj2#objet_nom
      | TYPE_VECTEUR ->
	   self#objet_nom^" is the image of "^obj1#objet_nom^" by the translation defined by "^obj2#objet_nom
      | _ -> assert false

    method recalcule =
      try
	let c1 = obj1#coordonnees_point in
	  (match obj2#objet_type with
	     | TYPE_POINT -> (* Symétrie centrale *)
		 let c2 = obj2#coordonnees_point in
		   self#change_parametres (COORDS_POINT (sym_point_point c1 c2))
	     | TYPE_VECTEUR -> (* Translation *)
		 let c2 = obj2#coordonnees_premier_point in
		 let c3 = obj2#coordonnees_second_point in
		   self#change_parametres (COORDS_POINT (v_minus c1 (v_minus c3 c2)))
	     | TYPE_DROITE | TYPE_SEGMENT -> (* Symétrie axiale *) 
		 let e2 = obj2#equation_droite in
		   self#change_parametres (COORDS_POINT (sym_point_line c1 e2))				 
	     | _ -> assert false);
	  self#rend_calculable
      with _ -> self#rend_incalculable
	    
		 
    method dependance = [obj1; obj2]
      
    method output_car_body () =
      Xml.Element("",[],[])
	
    method output_kig_body () =
      [Xml.Element("",[],[])]
	
    method output_drg_body () =
      Xml.Element("Transformation",[
		  ("name",self#objet_nom);   
		  ("of",obj1#objet_nom);
		  ("by",obj2#objet_nom);
		],[])

    method output_coq_body () = 
      match !!coq_geom_language with 
	  Narboux -> None	
	| Guilhot -> 
	    let p2 = Var(obj2#objet_nom) in
	    let p1 = Var(obj1#objet_nom) in
	    let aux p =  Some(Atom(R("=",[Var(self#objet_nom);p]))) in
	    begin
	      match obj2#objet_type with
		| TYPE_POINT -> (* Symétrie centrale *)
		    aux (Fn("symetrie",[p2;p1]))
		| TYPE_VECTEUR -> (* Translation *) 
		    let o2 = unpack (obj2#defining_points ()) in
		    let a = Var(fst o2) in
		    let b = Var(snd o2) in	   
		      aux (Fn("translation",[a;b;p1]))
		| TYPE_DROITE | TYPE_SEGMENT -> (* Symétrie axiale *) 
		    let o2 = unpack (obj2#defining_points ()) in
		    let a = Var(fst o2) in
		    let b = Var(snd o2) in	   
		      aux (Fn("reflexion",[a;b;p1]))
		| _ -> assert false
	    end


      
    method output_pst_eucl_body () = ""
      
    method output_eukleides_body () = 
      let c1 = obj1#coordonnees_point in
	match obj2#objet_type with
	  | TYPE_POINT -> (* Symétrie centrale *)
	      sprintf "%s = barycenter(%s,1,%s,-2)" self#objet_nom obj1#objet_nom obj2#objet_nom
	  | TYPE_VECTEUR -> (* Translation *)
	      sprintf "%s = translation(%s,%s)" self#objet_nom obj1#objet_nom obj2#objet_nom
	  | TYPE_DROITE | TYPE_SEGMENT -> (* Symétrie axiale *) 
	      sprintf "%s = reflection(%s,%s)" self#objet_nom obj1#objet_nom obj2#objet_nom
	  | _ -> assert false

    method predicate_form () = None (* TODO *)
	      
    initializer self#recalcule
  end

