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
open Objets_segment_deux_points
open Objets_vecteur_deux_points
open Objets_droite_orthogonale
open Objets_droite_parallele
open Objets_cercle_centre_point
open Objets_cercle_trois_points
open Objets_cercle_diametre
open Objets_point_transforme
open Objets_droite_transforme
open Objets_cercle_transforme
open Objets_texte_clique

let process_unknown s = 
  Printf.eprintf "Le tag %s n'est pas géré.\n" s
 
let init o xdata =
  let attrib s = attrib xdata s in
  let name = 
    try 
      attrib "name"
    with _ -> "default_name"
  in
  let color = black in  
  let style =
    match o#objet_type with
      TYPE_DROITE | TYPE_SEGMENT | TYPE_VECTEUR | TYPE_CERCLE | TYPE_REPERE ->
	STYLE_DROITE PLEINE	  
    | TYPE_POINT ->
	STYLE_POINT CARRE
    | TYPE_TEXTE ->
	STYLE_TEXTE {texte="toto"}
  in  
  o#nomme name;
  o#change_couleur color;
  o#change_style style;
  Document_managment.add_object o

let process_objet o = 
  let aux xml =
    match tag xml with     
    | "Point" -> 
	init (new objet_point_clique (Repere.random_point())) xml
    | "PointOn" -> 
	let o =  get_object (attrib xml "on") in
	begin
	  match o#objet_type with 
	  | TYPE_DROITE -> init (new objet_point_sur_droite o (random_point())) xml
	  | TYPE_CERCLE -> init (new objet_point_sur_cercle o (random_point())) xml
	  | _ -> assert false
	end
    | "Midpoint" -> 
	let o1 =  get_object (attrib xml "first") in
	let o2 = get_object (attrib xml "second") in
	init (new objet_point_milieu o1 o2) xml
    | "Segment" ->  
	let o1 =  get_object (attrib xml "from") in
	let o2 = get_object (attrib xml "to") in
	init (new objet_segment_deux_points o1 o2) xml
    | "Line" ->  
	let o1 =  get_object (attrib xml "from") in
	let o2 = get_object (attrib xml "to") in
	init (new objet_droite_deux_points o1 o2) xml
    | "Ray" ->  print_endline "Ray";
	(* TODO *)
	let o1 =  get_object (attrib xml "from") in
	let o2 = get_object (attrib xml "to") in
	init (new objet_droite_deux_points o1 o2) xml
    | "Polygon" as s ->  process_unknown s
    | "Circle" -> 
	let o1 =  get_object (attrib xml "midpoint") in
	let o2 = get_object (attrib xml "through") in
	init (new objet_cercle_centre_point o1 o2) xml
    | "Circle3" -> 
	let o1 =  get_object (attrib xml "from") in
	let o2 = get_object (attrib xml "midpoint") in
	let o3 = get_object (attrib xml "to") in
	init (new objet_cercle_trois_points o1 o2 o3) xml
    | "Plumb"  -> 
	let o1 =  get_object (attrib xml "point") in
	let o2 = get_object (attrib xml "line") in
	init (new objet_droite_orthogonale o1 o2) xml
    | "Parallel"  -> 
	let o1 =  get_object (attrib xml "point") in
	let o2 = get_object (attrib xml "line") in
	init (new objet_droite_parallele o1 o2) xml
    | "LineLineIntersection"  -> 
	let o1 =  get_object (attrib xml "first") in
	let o2 = get_object (attrib xml "second") in
	init (new objet_intersection_droite_droite o1 o2) xml
    | "CircleCircleIntersection"  -> 
	let o1 =  get_object (attrib xml "first") in
	let o2 = get_object (attrib xml "second") in
	let which = (attrib xml "which") in
	begin
	  match which with
	    "first" ->
	      init (new objet_intersection_cercles o2 o1 1) xml
	  | "second" ->
	      init (new objet_intersection_cercles o2 o1 (-1)) xml
	  | _ -> assert false
	end
    | "LineCircleIntersection"  -> 
       	let o1 =  get_object (attrib xml "line") in
	let o2 = get_object (attrib xml "circle") in
	let which = (attrib xml "which") in
	begin
	  match which with
	    "first" ->
	      init (new objet_intersection_cercle_droite o2 o1 1) xml
	  | "second" ->
	      init (new objet_intersection_cercle_droite o2 o1 (-1)) xml
	  | _ -> assert false
	end
    | _ as s -> Debug.pdb s; assert false
  in
  Xml.iter aux o


let process_facts o = 
  let aux xml =
    match tag xml with     
(*    | "Parallel" ->   	
	let o1 =  get_object (attrib xml "first") in
	let o2 = get_object (attrib xml "second") in
	init (new objet_texte_droites_par (random_point()) o1 o2) xml
    | "Perpendicular" ->
	let o1 =  get_object (attrib xml "first") in
	let o2 = get_object (attrib xml "second") in
	init (new objet_texte_droites_perp (random_point()) o1 o2) xml
   | "Collinear" ->
	let o1 =  get_object (attrib xml "first") in
	let o2 = get_object (attrib xml "second") in
	let o3 = get_object (attrib xml "third") in
	init (new objet_texte_alignement (random_point()) o1 o2 o3) xml
   | "EqualPoints" ->
	let o1 =  get_object (attrib xml "first") in
	let o2 = get_object (attrib xml "second") in
	init (new objet_texte_points_confondus (random_point()) o1 o2) xml *)
    | _ as s -> Debug.pdb s; assert false  (* TODO mettre pred dans text *)
  in
  Xml.iter aux o


let process_construction constr =
  let aux x = match tag x with
  | "Objects" -> process_objet x
  | "Comment" as s -> process_unknown s
  | s -> Debug.pdb s;assert false
  in
  Xml.iter aux constr
 


let process_constructions c =
  match tag c with
    "Construction" -> process_construction c
  | "Facts" -> process_facts c
  | _ -> assert false

let process_file doc =
  match tag doc with "ConstructionSequence" ->
    Xml.iter process_constructions doc
  |_ -> Debug.pdb (tag doc);assert false

