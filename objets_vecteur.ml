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

(* $Id: objets_vecteur.ml,v 1.22 2005/12/09 15:27:31 jnarboux Exp $ *)

open Options
open Optionsdeconfiguration
open Geometric_functions.GeometryFunctions
open Types_de_base
open Objets_graphiques
(* TO BE MODIFIED : les fonctions de calcul des lignes devraient
   être reportées dans un fichier commun *)
open Objets_ligne
open List
open Fonctions
open Repere
open Printf
open Fol
open Options
open Optionsdeconfiguration
open Formulas 
 
let dessiner_fleche win rep c eq = 
  let delta = v_scalar (of_int 10) eq.dir in
  let deltax = to_int delta.x in
  let deltay = to_int delta.y in
  let d = {xint = c.xint - deltax; yint = c.yint - deltay} in
  win#line ~x:(d.xint + deltay) ~y:(d.yint - deltax) ~x:c.xint ~y:c.yint;
  win#line ~x:c.xint ~y:c.yint ~x:(d.xint - deltay) ~y:(d.yint + deltax)
 
  
class type virtual t_objet_vecteur =
object 
  inherit Objets_graphiques.t_objet_graphique
  method objet_type : t_type_objet 
  method movable : unit -> bool
end

class virtual objet_vecteur=
object (self)
  inherit objet_graphique
    
  method objet_type = TYPE_VECTEUR
    
  method output_coq_body () =  
    match !!coq_geom_language with 
	Narboux -> None
      | Guilhot ->
	  Some(Atom(R("vec",[]))) 
	    
  method movable () = false
end


