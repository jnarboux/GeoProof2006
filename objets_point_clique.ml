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

open Options
open Optionsdeconfiguration
open Geometric_functions.GeometryFunctions
open Objets_graphiques
open Objets_point
open Types_de_base
open Fonctions
open Repere
open Printf

class type t_objet_point_clique =
object
  inherit Objets_point.t_objet_point
  method recalcule : unit
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list
  method def_langue_naturelle_corps : unit -> string
  method bouge :  t_coords -> unit
  method movable : unit -> bool
 
  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_coq_body : unit -> Fol.fol Formulas.formula option
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string

  method predicate_form : unit -> Fol.fol Formulas.formula option
end


class objet_point_clique coordsinit =
object (self)
  inherit objet_point as obp

  method movable () = true

  method def_langue_naturelle_corps () = self#objet_nom^" be a point"
 
  method output_car_body () =
    let c = self#coordonnees_point in
    Xml.Element("Point",
		[("name",self#objet_nom);
		 ("x",(to_string c.x !!precision)); 
		 ("y",(to_string c.y !!precision))	 
		 ],[])

  method output_kig_body () = 
    let to_string x = to_string x !!precision in 
    let c = self#coordonnees_point in
    [
     
     Xml.Element("Data",
		 [("type","double");
		  ("id",self#objet_nom^"x")
		],[Xml.PCData (to_string c.x)]);
     Xml.Element("Data",
		 [("type","double");
		  ("id",self#objet_nom^"y")
		],[Xml.PCData (to_string c.y)]);
     Xml.Element("Object",
		 [("type","FixedPoint");
		  ("id",self#objet_nom)
		],[
		 Xml.Element("Parent",[("id",self#objet_nom^"x")],[]);
		 Xml.Element("Parent",[("id",self#objet_nom^"y")],[])	 
	       ])
   ]
    
  method output_drg_body () = 
    let to_string x = to_string x !!precision in 
    let c = self#coordonnees_point in
    Xml.Element("Point",
		[("name",self#objet_nom);
		 ("x",(to_string c.x));
		 ("y",(to_string c.y))	 
	       ],[])

  method output_coq_body () = None

  method output_pst_eucl_body () = ""

  method output_eukleides_body () =     
    let to_string x = to_string x 5 in (* TODO precision *)
    let c = self#coordonnees_point in
    let x = to_string c.x in
    let y = to_string (neg c.y) in 
      (* The coordinate systems are not in the same direction *) 
      sprintf "%s = point(%s,%s)" self#objet_nom x y

  method recalcule = ()
  method dependance = []

  method bouge coords =
    self#change_parametres 
	(COORDS_POINT coords)

  method predicate_form () = None

  initializer
    self#bouge coordsinit
end


