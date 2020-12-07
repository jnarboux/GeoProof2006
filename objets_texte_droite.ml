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
open Objets_graphiques
open Objets_texte
open Fonctions
open Repere

class type t_objet_texte_droite =
object
  inherit t_objet_texte
  method recalcule : unit
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list
  method bouge : t_coords -> unit 
  
  method def_langue_naturelle_corps : unit -> string
  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
  method predicate_form : unit -> Fol.fol Formulas.formula option
end


class objet_texte_droite  d1 =
object (self)
  inherit objet_texte
    
  (* offset from point *)
  val mutable pos : t_coords =
    let origine = origine_ecran_t (Repere.get()) in
    let distance = of_int 7 in
    ecran_vers_papier {x=origine.x +! distance;y=origine.y -! distance} ()
      
  method def_langue_naturelle_corps () =
    let label = self#get_text () in
    label^" le texte associé au point "^d1#objet_nom

  method recalcule =   
    let c = d1#coordonnees_point
    in
      self#change_parametres
      (COORDS_POINT {x = c.x +! pos.x;y = c.y +! pos.y})

  method dependance = [d1]

  method output_car_body () =
      Xml.Element("",[],[])

  method output_kig_body () =
      [Xml.Element("",[],[])]
    
  method output_drg_body () =
    Xml.Element("",[],[])
 
  method output_eukleides_body () = ""
  method output_pst_eucl_body () = ""


  method bouge c =
    let p = d1#coordonnees_point in
    pos <- {x=c.x -! p.x;y=c.y -! p.y};
    self#recalcule

  method output_coq_body () = None 

  method predicate_form () = None

  initializer self#recalcule

end
