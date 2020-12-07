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
open Objets_droite
open Construction
open List
open Printf
open Fol
open Formulas

class type t_objet_droite_deux_points =
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
    method defining_points : unit -> (string * string) option
  end
 
class objet_droite_deux_points p1 p2 =
  object (self)
    inherit objet_droite

    method def_langue_naturelle_corps () = 
      self#objet_nom^" be the line ("^p1#objet_nom^","^p2#objet_nom^")"

    method recalcule =
      try
	let c1 = p1#coordonnees_point
	and c2 = p2#coordonnees_point in
	let eq = line_from_two_points c1 c2 in
	  self#rend_calculable;
	  self#change_parametres
	    (EQ_DROITE eq)
      with _ -> self#rend_incalculable
	  
    method dependance = [p1; p2]

    method output_car_body () =
      Xml.Element("",[],[])

    method output_kig_body () =
      [Xml.Element("",[],[])]
    
    method output_drg_body () =
      Xml.Element("Line",[
		  ("name",self#objet_nom); 
		  ("first",p1#objet_nom);
		  ("second",p2#objet_nom)
		],[])
	
    method output_coq_body () = None

    method output_pst_eucl_body () = ""

    method output_eukleides_body () = 
      sprintf "%s = line(%s,%s)" self#objet_nom p1#objet_nom p2#objet_nom

    method predicate_form () = Some(Not(Atom(R("=",[Var(p1#objet_nom);Var(p2#objet_nom)]))))
    method defining_points () = Some(p1#objet_nom,p2#objet_nom)

    initializer self#recalcule
  end

    
