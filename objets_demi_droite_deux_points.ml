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
open Options
open Optionsdeconfiguration
open Types_de_base
open Fonctions
open Repere
open Objets_graphiques
open Objets_ligne
open Objets_demi_droite
open Construction
open List
open Printf
open Fol
open Formulas

class type t_objet_demi_droite_deux_points =
object
  inherit Objets_demi_droite.t_objet_demi_droite
  method dessine :  GDraw.pixmap -> Repere.t -> unit
  method def_langue_naturelle_corps : unit -> string
  method recalcule : unit
(* TODO *)
(*  method depend_de : t_objet_graphique -> bool *)
  method dependance : t_objet_graphique list
  method est_proche : Repere.t -> t_coords -> bool
    
  method output_svg_body : unit -> Xml.xml
  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
(* TODO ? *)
  method output_coq_body : unit -> Fol.fol Formulas.formula option
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
(* TODO ? *)
  method predicate_form : unit -> Fol.fol Formulas.formula option
(* TODO ? *)
  method defining_points : unit -> (string * string) option
end
 
class objet_demi_droite_deux_points p1 p2 =
object (self)
  inherit objet_demi_droite
    
  method dessine win rep =
    let c1 = p1#coordonnees_point in
    let c2 = p2#coordonnees_point in
    let eq = line_from_two_points c1 c2 in
    let l = param_demi_droite_inter_cadre eq in
      dessiner_demi_droite_parametres win rep l eq

  method def_langue_naturelle_corps () =
    "la demi-droite ["^p1#objet_nom^","^p2#objet_nom^")"
	
  method recalcule =
    let c1 = p1#coordonnees_point in
    let c2 = p2#coordonnees_point in 
      self#change_parametres 
	(COORDS_DEUX_POINTS (c1,c2))	
	
  method dependance = [p1; p2]
			
  (* TODO *)
  method est_proche rep c =
    let c1 = p1#coordonnees_point in
    let c2 = p2#coordonnees_point in
    let eq = line_from_two_points c1 c2 in
    let c1 = papier_vers_ecran_t rep p1#coordonnees_point in
    let c2 = papier_vers_ecran_t rep p2#coordonnees_point in
    let between p q r = (q<!p && p<!r) || (r<!p && p<!q) in 
      distance2_point_droite_pixel rep c eq <= !!pixels_proche &&
      (between c.x c1.x c2.x) && (between c.y c1.y c2.y)

  (* TODO *)
  method  output_svg_body () = 
    let rep = (Repere.get()) in
    let c1 = papier_vers_ecran rep p1#coordonnees_point in
    let c2 = papier_vers_ecran rep p2#coordonnees_point in
      Xml.Element("line",
		  [("x1",(string_of_int c1.xint));
		   ("y1",(string_of_int c1.yint));
		   ("x2",(string_of_int c2.xint));
		   ("y2",(string_of_int c2.yint))
		  ],[])
	
  method output_car_body () =
    Xml.Element("",[],[])
      
  method output_kig_body () =
    [Xml.Element("",[],[])]
    
  (* TODO *)
  method output_drg_body () =
    Xml.Element("Demi-droite",[ 
		  ("name",self#objet_nom); 
		  ("first",p1#objet_nom);
		  ("second",p2#objet_nom)
		],[])

  (* TO BE VERIFIED *)
  method output_coq_body () = None

  method output_eukleides_body () = 
    sprintf "%s = halfline(%s,%s)" self#objet_nom p1#objet_nom p2#objet_nom
      
  method output_pst_eucl_body () = ""
				     
  (* TO BE VERIFIED *)
  method predicate_form () = Some(Not(Atom(R("=",[Var(p1#objet_nom);Var(p2#objet_nom)]))))

  (* TO BE VERIFIED *)
  method defining_points () = Some(p1#objet_nom,p2#objet_nom)

  initializer self#recalcule
end
