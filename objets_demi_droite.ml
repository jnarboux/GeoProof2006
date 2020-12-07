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

(* $Id: objets_demi_droite.ml,v 1.1 2005/10/29 02:38:59 eul_bofo Exp $  *)

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

let param_demi_droite_inter_cadre e =
  let l = List.filter (fun t -> ge t zero) (param_droite_inter_cadre e) in
    match length l with
      | 0 -> []
      | 1 -> zero::l
      | 2 -> l
      | _ -> assert false

let dessiner_demi_droite_parametres (win:GDraw.pixmap) rep l e =
  let c1 = papier_vers_ecran rep (point_sur_droite (hd l) e)
  and c2 = papier_vers_ecran rep (point_sur_droite (hd (tl l)) e)
  in
    win#line c1.xint c1.yint c2.xint c2.yint

class type virtual t_objet_demi_droite =
object
  inherit Objets_ligne.t_objet_ligne
  method objet_type : t_type_objet 
end
      
class virtual objet_demi_droite =
object (self)
  inherit objet_ligne
    
  method objet_type = TYPE_DEMI_DROITE
end
