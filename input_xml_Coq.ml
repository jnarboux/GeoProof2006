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

open Xml
open Couleurs
open Optionsdeconfiguration
open Types_de_base
open Objets_graphiques
open Construction
open Repere
open Objets_point_clique
open Objets_point_milieu
open Objets_point_sur_droite
open Objets_point_sur_cercle
open Objets_intersection_droites
open Objets_intersection_cercle_droite
open Objets_intersection_cercles
open Objets_droite_deux_points
open Objets_droite_mediatrice
open Objets_droite_bissectrice
open Objets_segment
open Objets_vecteur
open Objets_droite_orthogonale
open Objets_droite_parallele
open Objets_cercle_centre_point
open Objets_cercle_trois_points
open Objets_cercle_diametre
open Objets_point_transforme
open Objets_droite_transforme
open Objets_cercle_transforme
open Objets_texte_clique

let traite_inconnu s = 
  Printf.eprintf "Le tag %s n'est pas géré.\n" s
    
let init o name = 
  o#nomme name;  
  o#change_style (STYLE_POINT ROND);
  Document_managment.add_object o
  
let process_prod xml = 
  match tag xml with
    "decl" -> 
      begin
	(* Si y a un binder on suppose que c'est un point libre *)
	(* donc on fait un point au pif *)
	try 
	  let point_name = attrib xml "binder" in
	  init (new objet_point_clique (random_point())) point_name
	with
	  _ -> Debug.pdb "notapoint"
      end
  | "target" -> Debug.pdb "target"
  | _ -> assert false

let process_theorem xml =
  match tag xml with
    "PROD" -> 
      Debug.pdb "prod";
      Xml.iter process_prod xml
  | _ -> assert false

let process_file doc =
  match tag doc with "ConstantType" ->
    Xml.iter process_theorem doc
  |_ -> Debug.pdb (tag doc);assert false

