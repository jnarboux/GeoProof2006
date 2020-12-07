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
open Types_de_base
open Fonctions
open Repere
open Objets_graphiques
open Objets_point
open Construction
open Printf

class type t_object_segment_mark =
object
  inherit Objets_graphiques.t_objet_graphique
  method recalcule : unit
  method def_langue_naturelle_corps : unit -> string
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list

  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_coq_body : unit -> Fol.fol Formulas.formula option
  method output_svg_body : unit -> Xml.xml

  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string 
  method dessine :  GDraw.pixmap -> Repere.t -> unit 
  method est_proche : Repere.t -> t_coords -> bool 
  method objet_type : t_type_objet  
  method movable : unit -> bool
  method predicate_form : unit -> Fol.fol Formulas.formula option
  method defining_points : unit -> (string * string) option
end
 
class object_segment_mark s =
object (self)
  inherit objet_graphique

  method objet_type = TYPE_MARK 
      
  method dessine win rep =
    let c1 = s#coordonnees_premier_point in
    let c2 = s#coordonnees_second_point in
    let o = origine_ecran_t rep in
    let v_5 = ecran_vers_papier {x=o.x;y=o.y +! (of_int 7)} () in
    let i = mid_point c1 c2 in
    let v = v_minus c2 c1 in
    let g = v_plus (v_scalar (of_int 2) (v_norm v)) v in
    let h = v_scalar ((of_int 1) /! (norm g) *! (norm v_5)) g in 
    let a = v_plus i h in
    let b = v_minus i h in     
    let cb = papier_vers_ecran rep b in
    let ca = papier_vers_ecran rep a in
      win#line ca.xint ca.yint cb.xint cb.yint 

  method est_proche rep c = false

  method output_svg_body () = Xml.Element("",[],[])

  method def_langue_naturelle_corps () = 
    sprintf "%s une marque pour %s" self#objet_nom s#objet_nom
	 
  method recalcule =
    let c1 = s#coordonnees_premier_point
    and c2 = s#coordonnees_second_point
    in
      self#change_parametres
	(COORDS_DEUX_POINTS (c1,c2))	
	
  (* TODO real cordinates *)


  method dependance = [s]

  method output_car_body () =
    Xml.Element("",[],[])
      
  method output_kig_body () =
    [Xml.Element("",[],[])]
      
  method output_drg_body () =
    Xml.Element("Mark",
		[("name",self#objet_nom);
		 ("segment",s#objet_nom)	 
	       ],[])

  method output_coq_body () = None

  method output_eukleides_body () =
    sprintf "mark(%s)" s#objet_nom

  method output_pst_eucl_body () = ""
      
  method movable () = false

  method predicate_form () = None
  method defining_points () = None

  initializer 
    self#recalcule
end

