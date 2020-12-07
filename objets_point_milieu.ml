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
open Fonctions
open Repere
open Objets_graphiques
open Objets_point
open Construction
open Printf
open Fol
open Formulas
open Options
open Optionsdeconfiguration

class type t_objet_point_milieu =
object
  inherit Objets_point.t_objet_point
  method recalcule : unit
  method def_langue_naturelle_corps : unit -> string
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list

  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_coq_body : unit -> Fol.fol Formulas.formula option
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
  method predicate_form : unit -> Fol.fol Formulas.formula option

end
 
class objet_point_milieu p1 p2 =
object (self)
  inherit objet_point

  method def_langue_naturelle_corps () = 
    sprintf "%s be the midpoint of [%s,%s]" self#objet_nom p1#objet_nom p2#objet_nom
	 
  method recalcule =
    try
      let c1 = p1#coordonnees_point
      and c2 = p2#coordonnees_point
      in
	self#change_parametres
	  (COORDS_POINT {x = (c1.x +! c2.x) /! two;
			 y =  (c1.y +! c2.y) /! two});
	self#rend_calculable
    with _ -> self#rend_incalculable
	  
  method dependance = [p1; p2]

  method output_car_body () =
      Xml.Element("",[],[])

  method output_kig_body () =
      [Xml.Element("",[],[])]
    
  method output_drg_body () =
    Xml.Element("Midpoint",
		[("name",self#objet_nom);
		 ("First",p1#objet_nom);
		 ("Second",p2#objet_nom)	 
	       ],[])

  method output_coq_body () = 
    match !!coq_geom_language with
	Narboux -> self#pred_form "is_midpoint"
      | Guilhot ->  
	  Some(Atom(
		 R("=",[
		     Var(self#objet_nom);
		     Fn("milieu",[Var(p1#objet_nom);Var(p2#objet_nom)])
		   ])))
    
  method output_coq_tactic () =
    match !!coq_geom_language with
	Narboux -> Some (sprintf "midpoint %s %s %s" self#objet_nom p1#objet_nom p2#objet_nom)
      | Guilhot -> Some (sprintf "soit_milieu %s %s %s" self#objet_nom p1#objet_nom p2#objet_nom)

  method output_eukleides_body () =
    sprintf "%s = barycenter(%s,%s)" self#objet_nom p1#objet_nom p2#objet_nom
      
  method output_pst_eucl_body () = ""
    
  method private  pred_form pred = Some(Atom(R(pred,[Var(self#objet_nom);Var( p1#objet_nom);Var(p2#objet_nom)])))
    
  method predicate_form () = self#pred_form "is_midpoint"
      
  initializer 
    self#recalcule
end

