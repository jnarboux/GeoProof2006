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
open Objets_graphiques
open Objets_texte
open Construction
open Repere
open Fol
open Formulas

class type t_objet_texte_points_confondus =
object
  inherit Objets_texte.t_objet_texte

  method recalcule : unit
  method def_langue_naturelle_corps : unit -> string
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
 
class objet_texte_points_confondus coordsinit p1 p2 =
object (self)
  inherit objet_texte
  method def_langue_naturelle_corps () = ""
       

  method recalcule =
    let c1 = p1#coordonnees_point in
    let c2 = p2#coordonnees_point in
    if (equals_point c1 c2) then 
      self#set_text "These points are equals" else 
      self#set_text "These points are distinct"
  
  method bouge c =
    self#change_parametres (COORDS_POINT c);
  	  
  method dependance = [p1; p2]

  method output_car_body () =
      Xml.Element("",[],[])

  method output_kig_body () =
      [Xml.Element("",[],[])]
    
  method output_drg_body () =
    Xml.Element("TestPointEquality",
		[
		 ("name",self#objet_nom);   
		 ("first",p1#objet_nom);
		 ("second",p2#objet_nom); 
	       ],[])

  method output_eukleides_body () = ""
  method output_pst_eucl_body () = ""

  method predicate_form () = Some (Atom(R("=",[Var(p1#objet_nom);Var(p2#objet_nom)])))

 
  initializer 
    self#bouge coordsinit;
    self#recalcule

end
