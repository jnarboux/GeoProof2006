(*******************************************************************************)
(* GeoProof, an interactive geometry tool writen in OCaml.                    *)
(* Copyright (C) 2004 Nicolas Fran�ois et Julien Narboux                       *)
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
open Objets_cercle
open Fonctions
open Repere
open Printf
  
class type virtual t_objet_cercle_transforme =
  object 
    inherit Objets_cercle.t_objet_cercle
	
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
    method defining_points : unit -> (string*string) option     
  end
      
class objet_cercle_transforme obj1 obj2 =
  object (self)
    inherit objet_cercle

    method def_langue_naturelle_corps () = 
      match obj2#objet_type with
	TYPE_POINT | TYPE_DROITE | TYPE_SEGMENT ->
	  self#objet_nom^" be the symetric circle to "^obj1#objet_nom^" using "^obj2#objet_nom
      | TYPE_VECTEUR ->
	  self#objet_nom^" be the image of "^obj1#objet_nom^" by the translation defined by "^obj2#objet_nom
      | _ -> assert false

    method recalcule =
      try
	let c1 = obj1#equation_cercle in
	  (match obj2#objet_type with
	    | TYPE_POINT -> (* Sym�trie centrale *)
		let c2 = obj2#coordonnees_point in
		let c3 = sym_point_point  c1.center c2 in
		  self#change_parametres
		    (EQ_CERCLE {center = c3;
				radius = c1.radius})
	    | TYPE_VECTEUR -> (* Translation *)
		let c2 = obj2#coordonnees_premier_point
		and c3 = obj2#coordonnees_second_point in
		  self#change_parametres
		    (EQ_CERCLE {center = v_minus c1.center (v_minus c3 c2);
				radius = c1.radius})
	    | TYPE_DROITE | TYPE_SEGMENT -> (* Sym�trie axiale *)
		let e2 = obj2#equation_droite in
		let c3 = sym_point_line c1.center e2
		in
		  self#change_parametres
		    (EQ_CERCLE {center = c3;
				radius = c1.radius})
	    | _ -> assert false);
	  self#rend_calculable
      with _ -> self#rend_incalculable
	    
    method dependance = [obj1; obj2]

    method output_car_body () =
      Xml.Element("",[],[])
	
    method output_kig_body () =
      [Xml.Element("",[],[])]
	
    method output_drg_body () =
      Xml.Element("Transformation",
		  [
		   ("name",self#objet_nom);   
		   ("of",obj1#objet_nom);
		   ("by",obj2#objet_nom);
		 ],[])
	
    method output_coq_body () = None
    method output_pst_eucl_body () = ""

    method output_eukleides_body () = 
      match obj2#objet_type with
	| TYPE_POINT -> (* Sym�trie centrale *)
	    sprintf "%s = rotation(%s,%s)" 
	      (* Hack : by default eukleides rotation is 180 *)
	      self#objet_nom 
	      obj1#objet_nom 
	      obj2#objet_nom
	| TYPE_VECTEUR -> (* Translation *)
	    sprintf "%s = translation(%s,%s)" 
	      self#objet_nom 
	      obj1#objet_nom 
	      obj2#objet_nom
	| TYPE_DROITE | TYPE_SEGMENT -> (* Sym�trie axiale *)
	    sprintf "%s = reflection(%s,%s)" 
	      self#objet_nom 
	      obj1#objet_nom 
	      obj2#objet_nom
	| _ -> assert false
	    
    method predicate_form () = None (* TODO *)
    method defining_points () = None (* TODO *)

    initializer self#recalcule
  end

