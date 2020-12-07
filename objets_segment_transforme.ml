(*******************************************************************************)
(* GeoProof, an interactive geometry tool writen in OCaml.                    *)
(* Copyright (C) 2004 Julien Narboux                                           *)
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

open Types_de_base
open Objets_graphiques
open Objets_segment
open Fonctions
open Repere
  
class type virtual t_objet_segment_transforme =
  object 
    inherit Objets_segment.t_objet_segment
	
    method depend_de : t_objet_graphique -> bool
    method def_langue_naturelle_corps : unit -> string
    method dependance : t_objet_graphique list
    method recalcule : unit

    method output_car_body : unit -> Xml.xml
    method output_kig_body : unit -> Xml.xml list
    method output_drg_body : unit -> Xml.xml
    method output_coq_body : unit -> Xml.xml option
  end
      
class objet_segment_transforme obj1 obj2 =
  object (self)
    inherit objet_segment

    method def_langue_naturelle_corps () = ""

    method recalcule =
       let e1 = obj1#equation_droite in
      match obj2#objet_type with
      | TYPE_POINT ->
	  let c2 = obj2#coordonnees_point in
	  let c3 = sym_point_point {x = e1.origx; y = e1.origy} c2 in
	  self#change_parametres
	    (EQ_DROITE {origx = c3.x; origy = c3.y;
			dirx = -. e1.dirx;
			diry = -. e1.diry})
      | TYPE_VECTEUR ->
	  let c2 = obj2#coordonnees_premier_point  in
	  let c3 = obj2#coordonnees_second_point  in
 	  self#change_parametres
	    (EQ_DROITE {origx = e1.origx -. (c3.x -. c2.x);
			origy = e1.origy -. (c3.y -. c2.y);
			dirx = e1.dirx;
			diry = e1.diry})
      | TYPE_DROITE | TYPE_SEGMENT ->
	  let e2 = obj2#equation_droite in
	  let c3 = sym_point_droite {x = e1.origx; y = e1.origy} e2
	  and ps = prod_scal e1.dirx e1.diry e2.dirx e2.diry
	  in
	  self#change_parametres
	    (EQ_DROITE {origx = c3.x; origy = c3.y;
			dirx = 2. *. ps *. e2.dirx -. e1.dirx;
			diry = 2. *. ps *. e2.diry -. e1.diry})
      | _ -> assert false

    method dependance = [obj1; obj2]

    method output_car_body () =
      Xml.Element("",[],[])
	
    method output_kig_body () =
      [Xml.Element("",[],[])]
	
    method output_drg_body () =
      Xml.Element("",[],[])
	
    method output_coq_body () = None
	
    initializer self#recalcule
  end

