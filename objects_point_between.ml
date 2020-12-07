(*******************************************************************************)
(* GeoProof, an interactive geometry tool writen in OCaml.                    *)
(* Copyright (C) 2005 Julien Narboux                                           *)
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
open Construction
open Printf
open Fol
open Formulas

class type t_object_point_between =
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
 
class object_point_between (click:t_coords) s =
object (self)
  inherit objet_point

  method movable () = true
      
      (* we keep P1H/P1P2 *)
  val mutable pos = one /! two

  method def_langue_naturelle_corps () = self#objet_nom^" be a point on "^s#objet_nom
												  
  method recalcule =
    let c1 = s#coordonnees_premier_point
    and c2 = s#coordonnees_second_point
    in
    self#change_parametres
      (COORDS_POINT {x = (one -! pos) *! c1.x +! pos *! c2.x;
		     y = (one -! pos) *! c1.y +! pos *! c2.y})
      
  method dependance = [s]
      
  method output_car_body () =
    Xml.Element("",[],[])
      
  method output_kig_body () =
    [Xml.Element("",[],[])]
      
  method output_drg_body () =
    let to_string x = to_string x !!precision in 
    Xml.Element("PointOnSegment",
		[
		 ("name",self#objet_nom); 
		 ("segment",s#objet_nom);
		 ("position",to_string pos)
	       ],[])
      
  method output_coq_body () = None

  method bouge coords =
    let p1 = s#coordonnees_premier_point in
    let p2 = s#coordonnees_second_point in
    let h = projection_point_line_from_two_points coords p1 p2 in
    let lambda = 
      if between p1 h p2 then
	(distance p1 h) /! (distance p1 p2)
      else if between h p1 p2 then zero
      else one
    in
    pos <- lambda;
    self#recalcule      

  method output_pst_eucl_body () = ""

  method output_eukleides_body () =
    let to_string x = to_string x !!precision in 
    let pos = to_string pos in
      sprintf "%s = point(%s,%s)" self#objet_nom s#objet_nom pos

  method predicate_form () = Some(Atom(R("between",[Var(self#objet_nom);Var(s#objet_nom)])))
 
  initializer 
    self#bouge click

end

