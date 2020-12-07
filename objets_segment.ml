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

(* $Id: objets_segment.ml,v 1.22 2005/11/24 21:18:52 jnarboux Exp $  *)

open Geometric_functions.GeometryFunctions
open Options
open Optionsdeconfiguration
open Types_de_base
open Fonctions
open Repere
open Objets_graphiques
open Objets_ligne
open Construction
open List
open Printf

(* Dessine un segment étant donnés le repère et une liste de paramètres *)
let param_segment_inter_cadre c1 c2 =
  let e = line_from_two_points c1 c2 in
  let tf = parametre_de_point c2 e in
  let l = param_droite_inter_cadre e in
  let l' = List.filter (fun t -> (ge t zero) && (le t tf)) l in
    match length l' with
      | 0 -> [zero;tf]
      | 1 ->
	  if ((hd l) <! zero)
	  then [hd l';tf]
	  else zero::l' (* damn ! que c'est moche ! *)
      | 2 -> l
      | _ -> assert false

let dessiner_segment_parametres (win:GDraw.pixmap) rep l e =
  let c1 = papier_vers_ecran rep (point_sur_droite (hd l) e)
  and c2 = papier_vers_ecran rep (point_sur_droite (hd (tl l)) e)
  in
    win#line c1.xint c1.yint c2.xint c2.yint

let draw_segment_using_svg l e =
  let to_string  x =  to_string x !!precision in 
  let rep = Repere.get() in
  let c1 = papier_vers_ecran_t rep (point_sur_droite (hd l) e)
  and c2 = papier_vers_ecran_t rep (point_sur_droite (hd (tl l)) e)
  in
    Xml.Element("line",
		[ 
		  ("x1",(to_string c1.x));
		  ("x2",(to_string c2.x));
		  ("y1",(to_string c1.y));
		  ("y2",(to_string c2.y))
		],[])
    
class type virtual t_objet_segment =
object
  inherit Objets_ligne.t_objet_ligne
  method objet_type : t_type_objet 
end
      
class virtual objet_segment =
object (self)
  inherit objet_ligne
    
  method objet_type = TYPE_SEGMENT
end
