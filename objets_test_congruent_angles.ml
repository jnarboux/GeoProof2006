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

class type t_objet_test_congruent_angles =
object
  inherit Objets_texte.t_objet_texte

  method def_langue_naturelle_corps : unit -> string
  method recalcule : unit
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list
  method bouge : t_coords -> unit
  
  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
  method predicate_form : unit -> Fol.fol Formulas.formula option
end
 
class t coordsinit p1 p2 p3 p4 p5 p6 =
object (self)
  inherit objet_texte

  method def_langue_naturelle_corps () =
    "// congruent angles"

  method recalcule =
    let c1 = p1#coordonnees_point in
    let c2 = p2#coordonnees_point in
    let c3 = p3#coordonnees_point in
    let c4 = p4#coordonnees_point in
    let c5 = p5#coordonnees_point in
    let c6 = p6#coordonnees_point in
    if (congruent_angles c1 c2 c3 c4 c5 c6) then 
      self#set_text (i18n "Congruent angles") else 
      self#set_text (i18n "Not congruent angles")
  
  method bouge c =
    self#change_parametres (COORDS_POINT c)
  
  method dependance = [p1; p2 ; p3 ; p4; p5; p6]

  method output_car_body () =
      Xml.Element("",[],[])

  method output_kig_body () =
      [Xml.Element("",[],[])]
    
  method output_drg_body () =
    Xml.Element("TestCongruentAngles",
		[
		 ("name",self#objet_nom);   
		 ("Afirst",p1#objet_nom);
		 ("Asecond",p2#objet_nom);
		 ("Athird",p3#objet_nom);
		 ("Bfirst",p4#objet_nom);
		 ("Bsecond",p5#objet_nom);
		 ("Bthird",p6#objet_nom);
	       ],[])

  method output_eukleides_body () = ""
  method output_pst_eucl_body () = ""

  method predicate_form () = Some (Atom(R("eq_angles",[
					  Var(p1#objet_nom);
					  Var(p2#objet_nom);
					  Var(p3#objet_nom);
					  Var(p4#objet_nom);
					  Var(p5#objet_nom);
					  Var(p6#objet_nom)])))

  initializer 
    self#bouge coordsinit;
    self#recalcule
end
