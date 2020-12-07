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

class type t_object_point_left_turn =
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
 
class object_point_left_turn (click:t_coords) p1 p2 =
object (self)
  inherit objet_point
      
      (* let H be the projection of x on line p1p2
	 we keep Hx/p1p2 and  P1H/P1P2 *)
 
  val mutable pos = one,one

  method movable () = true


  method def_langue_naturelle_corps () = self#objet_nom^" be a point on the left of "^p1#objet_nom^" and "^p2#objet_nom
	 
  method recalcule =
    let p1 = p1#coordonnees_point in
    let p2 = p2#coordonnees_point in
    let h = {
      x = (one -! (snd pos)) *! p1.x +!(snd pos) *! p2.x;
      y = (one -! (snd pos)) *! p1.y +!(snd pos) *! p2.y;
       }
    in
    let v = {
      x = p2.y -! p1.y;
      y = p1.x -! p2.x
    }
    in
    self#change_parametres
      (COORDS_POINT {x = h.x +! (fst pos) *! v.x;
		     y = h.y +! (fst pos) *! v.y})
	  
  method dependance = [p1; p2]

  method bouge coords =
    let p1 = p1#coordonnees_point in
    let p2 = p2#coordonnees_point in
    let dist_p1p2 = algebraic_distance p1 p2 in
    let h = projection_point_line_from_two_points coords p1 p2 in
    if left_turn p1 p2 coords then
      let lambda1 = (distance h coords)  /! (distance p1 p2) in
      let lambda2 = (algebraic_distance p1 h) /! dist_p1p2 in
      pos <- (lambda1,lambda2);
      self#recalcule
    else
      let lambda1 = zero in
      let lambda2 = (algebraic_distance p1 h) /! dist_p1p2 in
      pos <- (lambda1,lambda2);
      self#recalcule
      
  method output_car_body () =
      Xml.Element("",[],[])

  method output_kig_body () =
      [Xml.Element("",[],[])]
    
  method output_drg_body () =
    let to_string x = to_string x !!precision in 
    Xml.Element("PointLeftTurn",
		[
		 ("name",self#objet_nom);
		 ("first",p1#objet_nom);
		 ("second",p2#objet_nom);
		 ("xpos",to_string (fst pos));
		 ("ypos",to_string (snd pos))
	       ],[])

  method output_coq_body () = None

  method output_pst_eucl_body () = ""

  method output_eukleides_body () = 
    let to_string x = to_string x !!precision in
    let c = self#coordonnees_point in
    let x = to_string c.x in
    let y = to_string (neg c.y) in 
      (* The coordinate systems are not in the same direction *) 
      sprintf "%s = point(%s,%s)" self#objet_nom x y

 method predicate_form () = Some(Atom(R("left_turn",[Var(self#objet_nom);Var(p1#objet_nom);Var(p2#objet_nom)])))
 

  initializer
    self#bouge click
end

