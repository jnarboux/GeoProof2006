(* $Id: objets_point_sur_droite.ml,v 1.24 2005/11/28 18:31:30 jnarboux Exp $ *)

(* Object : Point on a line *)

open Geometric_functions.GeometryFunctions
open Options 
open Optionsdeconfiguration
open Types_de_base
open Printf
open Options
open Optionsdeconfiguration
open Objets_graphiques 
open Objets_point 
open Fonctions
open Fol
open Formulas
  
class type t_objet_point_sur_droite =
object
  inherit Objets_point.t_objet_point
  method recalcule : unit
  method bouge : t_coords -> unit
  method depend_de : t_objet_graphique -> bool
  method def_langue_naturelle_corps : unit -> string
  method dependance : t_objet_graphique list
  method movable : unit -> bool
    
  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_coq_body : unit -> Fol.fol Formulas.formula option
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
  method predicate_form : unit -> Fol.fol Formulas.formula option
end
	  
class objet_point_sur_droite d c =
object (self)
  inherit objet_point
    
  method movable () = true
    
  method def_langue_naturelle_corps () = 
    self#objet_nom^" be a point on "^d#objet_nom
      
  val mutable t = zero
    
  method recalcule =
    try
      let eq = d#equation_droite in
	self#rend_calculable;    
	self#change_parametres
	  (COORDS_POINT (v_plus eq.orig (v_scalar t eq.dir)))  
    with  _ -> self#rend_incalculable
      
  method bouge c =
    let eq = d#equation_droite in
      t <- dot_product
	(v_minus c eq.orig) eq.dir;
      self#recalcule
	
  method dependance = [d]
    
  method output_car_body () =
    Xml.Element("",[],[])
	
  method output_kig_body () =
    [Xml.Element("",[],[])]
      
  method output_drg_body () = 
    let to_string x = to_string x !!precision in 
      Xml.Element("PointOnLine",
		  [
		    ("name",self#objet_nom);
		    ("line",d#objet_nom);
		    ("position",to_string t)
		  ],[])
	
  method output_pst_eucl_body () = ""
  
  method output_eukleides_body () = 
    let to_string x = to_string x !!precision in 
    let pos = to_string t in
      sprintf "%s = point(%s,%s)" self#objet_nom d#objet_nom pos

  method private predicate_form_generic pred () =
      let a = Var(fst (unpack (d#defining_points ()))) in
      let b = Var(snd (unpack (d#defining_points ()))) in      
	Some(Atom(R(pred,[Var(self#objet_nom);a;b])))
 
  method output_coq_body () = 
    match !!coq_geom_language with 
	Narboux -> self#predicate_form_generic "on_line" ()
      | Guilhot -> self#predicate_form_generic "alignes" ()


  method output_coq_tactic () = 
    match !!coq_geom_language with 
	Narboux -> 
	  let a,b = unpack (d#defining_points ()) in
	    Some (sprintf "point_on_line %s %s %s" self#objet_nom a b)
      | Guilhot -> None
	  
  method predicate_form () = 
    self#predicate_form_generic "collinear" ()
      
  initializer
    self#bouge c
end
  
  
(*

 GeoProof, an interactive geometry tool writen in OCaml.                   
 Copyright (C) 2004 Nicolas François et Julien Narboux                      
                                                                            
 This program is free software; you can redistribute it and/or              
 modify it under the terms of the GNU General Public License                
 as published by the Free Software Foundation; either version 2             
 of the License, or (at your option) any later version.                     
                                                                            
 This program is distributed in the hope that it will be useful,            
 but WITHOUT ANY WARRANTY; without even the implied warranty of             
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              
 GNU General Public License for more details.                               
                                                                            
 You should have received a copy of the GNU General Public License          
 along with this program; if not, write to the Free Software                
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)
  
