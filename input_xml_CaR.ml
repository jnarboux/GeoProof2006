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
open Xml
open Couleurs
open Optionsdeconfiguration
open Types_de_base
open Objets_graphiques
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

let traite_inconnu s = 
  Printf.eprintf "Le tag %s n'est pas géré.\n" s
    
let couleur_de_numero i = match i with
  1 -> green
| 2 -> blue
| 3 -> yellow
| _ -> red

let epaisseur_of_string s = match s with
  "thick" -> 4
| "thin" -> 1 
| "invisible" -> 2
| s -> Debug.pdb ("Epaisseur inconnue :"^s);assert false

let line_style_of_string s =
  match s with
    "invisible" 
  | "thick" -> STYLE_DROITE PLEINE
  | "thin" -> STYLE_DROITE POINTILLEE
  | s -> Debug.pdb (s^"unknown!\n");assert false
	
let point_style_of_string s =
  match s with
  | "circle" -> STYLE_POINT RONDPLEIN
  | "dcross" -> STYLE_POINT CROIX
  | "cross" -> STYLE_POINT CROIX
  | "diamond" -> STYLE_POINT CARRE
  | s -> Debug.pdb (s^"unknown!\n");assert false
       

let init o xdata =
  let attrib s = attrib xdata s in
  let nom = 
    try 
      attrib "name"
    with _ -> "default_name"
  in
  let couleur =
    try 
      couleur_de_numero (int_of_string (attrib "color"))
    with _ -> black
  in
  let visi =
    try 
      not (bool_of_string (attrib "hidden"))
    with _ -> true
  in
  let epaisseur =
    try 
      epaisseur_of_string (attrib "type")
    with
      _ -> 2  
  in
  let style =
    match o#objet_type with
      TYPE_DROITE | TYPE_SEGMENT | TYPE_VECTEUR | TYPE_CERCLE | TYPE_REPERE ->
	begin
	  try 
	    line_style_of_string (attrib "type")
	  with
	    _ -> 	    
	      STYLE_DROITE PLEINE
	end
    | TYPE_POINT ->
	begin
	  try 
	    point_style_of_string (attrib "shape")
	  with
	    _ -> 	    
	      STYLE_POINT CARRE
	end
    | TYPE_TEXTE ->
	STYLE_TEXTE {texte="toto"}
  in  
  o#nomme nom;
  o#change_couleur couleur;
  o#change_visibilite visi;
  o#change_epaisseur epaisseur;
  o#change_style style;
  Document_managment.add_object o

(* Fonctions spéciales pour les intersections doubles 
(i.e. impliquant au moins un cercle) 
on rajoute un objet dont on connait que le nom, 
les attributs sont mis à jour ensuite *)

let init_other o nom =
  o#nomme nom;
  Document_managment.add_object o


    
let init_other_att xdata =
  let nom = attrib xdata "name" in
  let o = Construction.get_object nom in
  let attrib s = attrib xdata s in
  let couleur =
    try 
      couleur_de_numero (int_of_string (attrib "color"))
    with _ -> black
  in
  let visi =
    try 
      not (bool_of_string (attrib "hidden"))
    with _ -> true
  in
  let style =
    match o#objet_type with
      TYPE_DROITE | TYPE_SEGMENT | TYPE_CERCLE | TYPE_VECTEUR | TYPE_REPERE ->
	begin
	  try 
	    line_style_of_string (attrib "type")
	  with
	    _ -> 	    
	      STYLE_DROITE PLEINE
	end
    | TYPE_POINT ->
	begin
	  try 
	    point_style_of_string (attrib "shape")
	  with
	    _ -> 	    
	      STYLE_POINT CROIX
	end
    | TYPE_TEXTE -> STYLE_TEXTE {texte="Text"}
  in
  o#change_couleur couleur;
  o#change_visibilite visi;
  o#change_style style

let traite_objet o = 
  let aux xml =
    match tag xml with     
      "Text" -> 
	let x = of_string (attrib xml "x") in
	let y = of_string (attrib xml "y") in
	init (new objet_texte_clique {x=x;y=y} "text") xml (* TODO put the text *)
    | "Expression" as s -> traite_inconnu s
    | "Track" as s -> traite_inconnu s
    | "Point" -> 
	let x = (of_string (attrib xml "x")) in
	let y = (of_string (attrib xml "y")) in
	init (new objet_point_clique {x=x;y=y}) xml
    | "PointOn" -> 
	let o =  Construction.get_object (attrib xml "on") in
	begin
	  match o#objet_type with 
	  | TYPE_DROITE -> init (new objet_point_sur_droite o (random_point())) xml
	  | TYPE_CERCLE -> init (new objet_point_sur_cercle o (random_point())) xml
	  | _ -> assert false
	end
    | "Midpoint" -> 
	let o1 =  Construction.get_object (attrib xml "first") in
	let o2 = Construction.get_object (attrib xml "second") in
	init (new objet_point_milieu o1 o2) xml
    | "Segment" ->  
	let o1 =  Construction.get_object (attrib xml "from") in
	let o2 = Construction.get_object (attrib xml "to") in
	init (new objet_segment_deux_points o1 o2) xml
    | "Line" ->  
	let o1 =  Construction.get_object (attrib xml "from") in
	let o2 = Construction.get_object (attrib xml "to") in
	init (new objet_droite_deux_points o1 o2) xml
    | "Ray" ->  print_endline "Ray";
	(* TODO *)
	let o1 =  Construction.get_object (attrib xml "from") in
	let o2 = Construction.get_object (attrib xml "to") in
	init (new objet_droite_deux_points o1 o2) xml
    | "Polygon" as s ->  traite_inconnu s
    | "Circle" -> 
	let o1 =  Construction.get_object (attrib xml "midpoint") in
	let o2 = Construction.get_object (attrib xml "through") in
	init (new objet_cercle_centre_point o1 o2) xml
    | "Circle3" -> 
	let o1 =  Construction.get_object (attrib xml "from") in
	let o2 = Construction.get_object (attrib xml "midpoint") in
	let o3 = Construction.get_object (attrib xml "to") in
	init (new objet_cercle_trois_points o1 o2 o3) xml
    | "Plumb"  -> 
	let o1 =  Construction.get_object (attrib xml "point") in
	let o2 = Construction.get_object (attrib xml "line") in
	init (new objet_droite_orthogonale o1 o2) xml
    | "Angle" as s -> traite_inconnu s
    | "Parallel"  -> 
	let o1 =  Construction.get_object (attrib xml "point") in
	let o2 = Construction.get_object (attrib xml "line") in
	init (new objet_droite_parallele o1 o2) xml
    | "Intersection"  -> 
	let o1 =  Construction.get_object (attrib xml "first") in
	let o2 = Construction.get_object (attrib xml "second") in
	let traite_intersection_double createur =
	  try
	    let which = (attrib xml "which") in
	    match which with
	      "first" ->
		init (createur o2 o1 1) xml
	    | "second" ->
		init (createur o2 o1 (-1)) xml
	    | _ -> assert false
	  with
	    _ ->
	      let nom_other = (attrib xml "other") in
	      init (createur o2 o1 1) xml;
	      init_other (createur o2 o1 (-1)) nom_other
	in
	begin
	  match (o1#objet_type,o2#objet_type) with
	  | (TYPE_DROITE,TYPE_DROITE) -> 
	      init (new objet_intersection_droite_droite o1 o2) xml
	  | (TYPE_DROITE,TYPE_CERCLE) ->
	       traite_intersection_double (new objet_intersection_cercle_droite)
	  | (TYPE_CERCLE,TYPE_CERCLE) ->
	      traite_intersection_double (new objet_intersection_cercles)
	  | _ -> assert false
	end
    | "OtherIntersection"  -> 
	init_other_att xml
    | _ as s -> Debug.pdb s; assert false
  in
  Xml.iter aux o

let traite_construction constr =
  let aux x = match tag x with
      "Window" as s -> traite_inconnu s
    | "Animate" as s -> traite_inconnu s 
    | "Track" as s -> traite_inconnu s
    | "Objects" -> traite_objet x
    | "Comment" as s -> traite_inconnu s
    | "Assignment" as s -> traite_inconnu s 
    | "Parameter" as s -> traite_inconnu s 
    | "Restrict" as s -> traite_inconnu s	
    | s -> Debug.pdb s;assert false
  in
    iter aux constr
 
let traite_constructions c =
  match tag c with
      "Construction" -> traite_construction c
    | "Macro" as s -> traite_inconnu s
    | _ -> assert false
	
let process_file doc =
  match tag doc with "CaR" ->
    iter traite_constructions doc
    |_ -> Debug.pdb (tag doc);assert false
	 
