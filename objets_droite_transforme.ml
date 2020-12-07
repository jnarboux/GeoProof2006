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
open Objets_droite
open Fonctions
open Repere
open Printf
  
class type virtual t_objet_droite_transforme =
  object 
    inherit Objets_droite.t_objet_droite
	
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
      
class objet_droite_transforme obj1 obj2 =
  object (self)
    inherit objet_droite

    method def_langue_naturelle_corps () = 
      match obj2#objet_type with
	TYPE_POINT | TYPE_DROITE | TYPE_SEGMENT ->
	  self#objet_nom^" be the symetric line to "^obj1#objet_nom^" using "^obj2#objet_nom
      | TYPE_VECTEUR ->
	   self#objet_nom^" be the image of "^obj1#objet_nom^" by the translation defined by "^obj2#objet_nom
      | _ -> assert false

    method recalcule =
      try
	let e1 = obj1#equation_droite in
	  (match obj2#objet_type with
	    | TYPE_POINT ->
		let c2 = obj2#coordonnees_point in
		let c3 = sym_point_point e1.orig c2 in
		  self#change_parametres
		    (EQ_DROITE {orig = c3;
				dir = v_neg e1.dir})
	    | TYPE_VECTEUR ->
		
		let c2 = obj2#coordonnees_premier_point  in
		let c3 = obj2#coordonnees_second_point  in
 		  self#change_parametres
		    (EQ_DROITE {orig= v_minus e1.orig (v_minus c3 c2);dir= e1.dir})
		    (*
		      {origx = e1.origx -. (c3.x -. c2.x);
		      origy = e1.origy -. (c3.y -. c2.y);
		      dirx = e1.dirx;
		      diry = e1.diry}) *)
	    | TYPE_DROITE | TYPE_SEGMENT ->
		let e2 = obj2#equation_droite in
		let c3 = sym_point_line e1.orig e2
		and ps = dot_product e1.dir e2.dir
		in
		  self#change_parametres
		    (EQ_DROITE {orig = c3;
				dir = (v_minus (v_scalar (two *! ps) e2.dir) e1.dir)})
		    (*
		      dirx = 2. *. ps *. e2.dirx -. e1.dirx;
		      diry = 2. *. ps *. e2.diry -. e1.diry})
		    *)
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
	| TYPE_POINT ->
	    sprintf "%s = rotation(%s,%s)" 
	      (* Hack : by default eukleides rotation is 180 *)
	      self#objet_nom 
	      obj1#objet_nom 
	      obj2#objet_nom
	| TYPE_VECTEUR ->
	    sprintf "%s = translation(%s,%s)" 
	      self#objet_nom 
	      obj1#objet_nom 
	      obj2#objet_nom
	| TYPE_DROITE | TYPE_SEGMENT ->
	    sprintf "%s = reflection(%s,%s)" 
	      self#objet_nom 
	      obj1#objet_nom 
	      obj2#objet_nom
	| _ -> assert false

    method predicate_form () = None (* TODO *)
    method defining_points () = None (* TODO *)
     
    initializer self#recalcule
  end

