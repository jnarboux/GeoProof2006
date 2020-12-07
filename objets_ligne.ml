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
open Repere
open Options
open Optionsdeconfiguration
open Objets_graphiques
open Construction
open List

(* Les lignes sont représentées en interne par un point et
   un vecteur directeur unitaire (type t_equation_droite). *)
   
let evalue_point p e = 
  star_product (v_minus p e.orig) (e.dir) 
(*
  ((p.x -. e.origx) *. e.diry -. (p.y -. e.origy) *. e.dirx)
*)

(* let coupe_bord p1 p2 e =
   ((evalue_point p1 e) *! (evalue_point p2 e) <! zero) *)

(* ibv et ibh renvoient une liste contenant (éventuellement)
   le paramètre du point d'intersection d'une ligne avec un
   bord de la fenêtre graphique *)
let ibv x e mini maxi =
  (* Intersection bord vertical *)
  try 
    if e.dir.x =! zero then raise No_Solution;
    let t = (x -! e.orig.x) /! e.dir.x in
    let y = e.orig.y +! t *! e.dir.y in
      if ((y >! mini) && (y <! maxi)) then [t]
      else []
  with _ -> []

let ibh y e mini maxi =
  (* Intersection bord horizontal *)
  try
    let t = (y -! e.orig.y) /! e.dir.y in
    let x = e.orig.x +! t *! e.dir.x in
      if ((x >! mini) && (x <! maxi)) then [t]
      else []
  with _ -> []
    
(* param_droite_inter_cadre renvoie 0 ou 2 parametres *)
(* de points d'intersection avec le bord *)

let param_droite_inter_cadre e =
  let xmin = x_min ()
  and xmax = x_max ()
  and ymin = y_min ()
  and ymax = y_max ()
  in
  let l = sort (compare !!precision) 
    ((ibv xmin e ymin ymax) @
       (ibh ymin e xmin xmax) @
       (ibv xmax e ymin ymax) @
       (ibh ymax e xmin xmax))
  in
    match length l with
      | (0 | 1) -> []
      | 2 -> l
      | (3 | 4) -> (* cas ou la droite coupe en un ou deux coins *)
	  [hd l; hd (rev l)] (* simpliste, mais ça marche à peu près *)
      | _ -> assert false
	  
(* droite_inter_cadre renvoie 0 ou 2 couples de *)
(* coordonnees de points d'intersection avec le bord *)

let point_sur_droite t e = v_plus e.orig (v_scalar t e.dir)

let parametre_de_point c e =
  try
    (c.x -! e.orig.x) /! e.dir.x
  with Division_by_zero -> (c.y -! e.orig.y) /! e.dir.y

let droite_inter_cadre rep e =
  map (function t -> papier_vers_ecran rep (point_sur_droite t e))
    (param_droite_inter_cadre e)

let distance_point_droite p e = abs (evalue_point p e)

let distance2_point_droite_pixel rep p e =
  let p = ecran_vers_papier p () in
  let alpha = rep.d.x in
  let beta = (distance_point_droite p e) *! alpha
  in
    to_int (beta *! beta)

class type virtual t_objet_ligne =
object
  inherit Objets_graphiques.t_objet_graphique
  method movable : unit -> bool
end

class virtual objet_ligne =
object (self)
  inherit objet_graphique
    
  method movable () = false
end

