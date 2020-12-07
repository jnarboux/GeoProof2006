(* $Id *)

open Geometric_functions.GeometryFunctions 
open Objets_graphiques 
open Objets_droite
open Printf
open Fol
open Fonctions
open Formulas  
open Options
open Optionsdeconfiguration
open Types_de_base

class type t_objet_orthogonale =
object
  inherit Objets_droite.t_objet_droite
    
  method def_langue_naturelle_corps : unit -> string 
  method recalcule : unit
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list
    
  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_coq_body : unit -> Fol.fol Formulas.formula option
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
  method predicate_form : unit -> Fol.fol Formulas.formula option    
  method defining_points : unit -> (string*string) option
end
	  
class objet_droite_orthogonale p d =
object (self)
  inherit objet_droite
    
  method def_langue_naturelle_corps () = 
    self#objet_nom^" be the line perpendicular to "^d#objet_nom^" going through "^p#objet_nom
      
  method recalcule =
    try
      let co = p#coordonnees_point
      and eq = d#equation_droite in
	self#change_parametres
	  (EQ_DROITE (line_perpendicular co eq));
	self#rend_calculable
    with _ -> self#rend_incalculable

  method dependance = [p; d]
    
  method output_car_body () =
    Xml.Element("",[],[])
      
  method output_kig_body () =
    [Xml.Element("",[],[])]
      
  method output_drg_body () =
    Xml.Element("Perpendicular",[ 
		  ("name",self#objet_nom); 
		  ("point",p#objet_nom);
		  ("line",d#objet_nom)
		],[])
      
  method output_coq_body () = 
    match !!coq_geom_language with 
	Narboux -> None
      | Guilhot ->
	  let p = unpack (self#defining_points ()) in
	  let q = unpack (d#defining_points ()) in
	  let c = Var(fst p) in
	  let d = Var(snd p) in
	  let a = Var(fst q) in
	  let b = Var(snd q) in
	    Some(Atom(R("perpendiculaires",[Fn("droite",[d;c]);Fn("droite",[a;b])]))) 

  method output_pst_eucl_body () = ""

  method output_eukleides_body () = 
    sprintf "%s = perpendicular(%s,%s)" 
      self#objet_nom 
	d#objet_nom 
	p#objet_nom

  method predicate_form () = 
    let p = unpack (self#defining_points ()) in
    let q = unpack (d#defining_points ()) in
    let c = Var(fst p) in
    let d = Var(snd p) in
    let a = Var(fst q) in
    let b = Var(snd q) in
      Some(And(Atom(R("perpendicular",[a;b;c;d])),Not(Atom(R("=",[c;d])))))
	
  method defining_points () = Some(p#objet_nom,self#objet_nom^"_b")


  initializer self#recalcule
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
  
