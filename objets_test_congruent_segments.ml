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
open Objets_graphiques
open Objets_texte
open Construction
open List
open Repere
open I18n
open Fol
open Formulas
open Mark_management

class type t_objet_test_congruent_segments =
object
  inherit Objets_texte.t_objet_texte

  method def_langue_naturelle_corps : unit -> string
  method recalcule : unit
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list
  method bouge : t_coords -> unit
  method dessine :  GDraw.pixmap -> Repere.t -> unit

  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
  method predicate_form : unit -> Fol.fol Formulas.formula option
end
 
class objet_test_congruent_segments  s1 s2 =
object (self)
  inherit objet_graphique

  val mutable cong = false 

  method def_langue_naturelle_corps () =
    "// test if the segments are congruent"

  method dessine win rep =
    if cong then
      begin
	let m = compute_mark self#objet_nom s1 s2 () in
	  self#change_style (STYLE_MARK m);
	  Debug.pdb "changestyle :";
	  (match m with LINE(n) -> Debug.pdb (string_of_int n));
	  draw_mark win rep m s1;
	  draw_mark win rep m s2
      end

  method est_proche _ _ = false
    
  method recalcule =
    let c1 = s1#coordonnees_premier_point in
    let c2 = s1#coordonnees_second_point in
    let c3 = s2#coordonnees_premier_point in
    let c4 = s2#coordonnees_second_point in
    cong <- (congruent_segments c1 c2 c3 c4) 
  
  method bouge c = ()
  
  method dependance = [s1; s2]

  method output_svg_body () = 
    Xml.Element("",[],[])

  method output_coq_body () = 
    None


  method output_car_body () =
    Xml.Element("",[],[])

  method output_kig_body () =
      [Xml.Element("",[],[])]
    
  method output_drg_body () =
    Xml.Element("TestCongurentSegments",
		[
		 ("name",self#objet_nom);
		 ("first",s1#objet_nom);
		 ("second",s2#objet_nom)
	       ],[])

  method output_eukleides_body () = ""
  method output_pst_eucl_body () = ""

  method objet_type = TYPE_MARK
  method movable () = false

  method predicate_form () = Some (Atom(R("eq_lengths",[Var(s1#objet_nom);Var(s2#objet_nom)])))
  method defining_points () = None

  initializer 
    self#recalcule
end
