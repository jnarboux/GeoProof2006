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
open Types_de_base
open Objets_graphiques
open Construction
open Repere
open Objets_repere
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
open Objets_texte_point
open Objects_point_between
open Objects_point_left_turn
open Optionsdeconfiguration
open Geometric_functions.GeometryFunctions
open Input_exceptions

let traite_inconnu s = 
  Printf.eprintf "Le tag %s n'est pas géré.\n" s

   
let color_of_string s =
  match s with
      "" -> blue
    | "#ffff00" -> yellow
    | "#00ffff" -> blue
    | "#800000" -> orange
    | "#a0a0a4" -> grey
    | "#00ff00" -> green
    | _ -> black
      

let line_style_of_string s =
  match s with
    "SolidLine" -> STYLE_DROITE PLEINE
  | "DashDotDotLine" 
  | "DashLine" 
  | "DashDotLine"
  | "DotLine" -> STYLE_DROITE POINTILLEE
  | s -> Debug.pdb (s^"unknown!\n");assert false

let point_style_of_string s =
  match s with
  | "Round" -> STYLE_POINT RONDPLEIN
  | "Cross" -> STYLE_POINT CROIX
  | "RoundEmpty" -> STYLE_POINT ROND
  | "RectangularEmpty" -> STYLE_POINT CARRE
  | "Rectangular" -> STYLE_POINT CARREPLEIN
  | s -> Debug.pdb (s^"unknown!\n");assert false
       
let bool_of_string s =
  match s with
      "true" -> true
    | "false" -> false
    | s -> 
	Debug.pdb "This string does not represent a boolean";
	Debug.pdb s;
	assert false

let process_attribut xdata =
  Debug.pdb "process attributs";
  Debug.pdb (Xml.to_string xdata);
  let attrib s = attrib xdata s in
  let o = try
    get_object (attrib "name")
  with _ -> 
    Debug.pdb "Object not found :";
    Debug.pdb (attrib "name");
    assert false 
  in 
    Debug.pdb (attrib "name");
    
    let visi =
    try 
      bool_of_string (attrib "shown")
    with _ -> true
  in
  let style =
    match o#objet_type with
	TYPE_DROITE | TYPE_SEGMENT | TYPE_CERCLE | TYPE_REPERE | TYPE_VECTEUR ->
	begin
	  try 
	    line_style_of_string (attrib "line-style")
	  with
	    _ -> 	    
	      STYLE_DROITE PLEINE
	end
      | TYPE_POINT ->
	begin
	  try 
	    point_style_of_string (attrib "point-style")
	  with
	    _ -> 	    
	      STYLE_POINT CROIX
	end
      | TYPE_TEXTE ->
	  begin
	    try 
	      STYLE_TEXTE {texte = (attrib "text")}
	    with _ ->
	      STYLE_TEXTE {texte="unknown"}
	  end
(*
      | TYPE_REPERE -> (* match case not used TODO revoir la structure pour les styles *)
	  STYLE_REPERE {grid_shown = bool_of_string (attrib "grid-style")}*)
      | TYPE_MARK ->
	  STYLE_MARK (LINE 1) (* TODO *)
  in
  let width =
    try 
      let e = int_of_string (attrib "width") in
	if e <= 0 then 2 else e
    with
	_ -> 2
  in
    Debug.pdb "change style";
    (* o#change_couleur color; *)
    o#change_visibilite visi;
    o#change_style style;
    o#change_epaisseur width;
    Debug.pdb "style done";
    let aux x =
      match tag x with 
	  "Color" ->
	    let attrib s = Xml.attrib x s in
	    let red = int_of_string (attrib "red") in
	    let green  = int_of_string (attrib "green") in
	    let blue  = int_of_string (attrib "blue") in
	    let color = color_of_rgb red green blue in
	      o#change_couleur color
	| _ -> ()
    in
      Xml.iter aux xdata


let init o xml = 
  let nom = 
    try 
      attrib xml "name"
    with _ -> "default_name"
  in
    o#nomme nom;
    Debug.pdb nom;
    Document_managment.add_object o
 
let process_hierarchy xml =
  Debug.pdb "process hierachy";
  Debug.pdb (Xml.to_string xml);
  let aux xml =
    let attr = attrib xml in       
    let gattr s = 
      try get_object (attr s) with
	  Not_found -> raise (Undefined_Object (attr s))
    in
      match tag xml with     
	| "Axes" -> 
	  (* We assume that there is always a object Grid *)
	    if not (attrib xml "name" = "Grid") then
	      begin
		let o = new objet_repere in
		  init o xml;
		  o#change_style (STYLE_REPERE 
				    {grid_shown= bool_of_string (attrib xml "grid_shown")})
		    
	      end
	| "Text" -> 
	let x = of_string (attr "x") in
	let y = of_string (attr "y") in
	  init (new objet_texte_clique {x=x;y=y} "text") xml
    | "PointLabel" -> 
	let o =  gattr "point" in
	init (new objet_texte_point o "text") xml
    | "Point" -> 
	Debug.pdb "point";
	let x = of_string (attr "x") in
	let y = of_string (attr "y") in
	init (new objet_point_clique {x=x;y=y}) xml
    | "PointOnLine" -> 
	let o =  gattr "line" in
 (* TODO put the right ratio *)
	init (new objet_point_sur_droite o (random_point())) xml
    | "PointOnCircle" -> 
	let o =  gattr "circle" in
	init (new objet_point_sur_cercle o (random_point())) xml
    | "PointOnSegment" -> 
	let o =  gattr "segment" in
	init (new object_point_between (random_point()) o) xml
    | "PointLeftTurn" -> 
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new object_point_left_turn  (random_point()) o1 o2) xml
    | "Midpoint" -> 
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_point_milieu o1 o2) xml
    | "Segment" ->  
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_segment_deux_points o1 o2) xml
    | "Vector" ->  
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_vecteur_deux_points o1 o2) xml	      
    | "Line" ->  
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_droite_deux_points o1 o2) xml
    | "CircleCenterPoint" -> 
	let o1 = gattr "center" in
	let o2 = gattr "through" in
	init (new objet_cercle_centre_point o1 o2) xml 
    | "CircleDiameter" -> 
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_cercle_diametre o1 o2) xml
    | "CircleThreePoints" -> 
	Debug.pdb "circlethreepoints";
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	let o3 = gattr "third" in
	init (new objet_cercle_trois_points o1 o2 o3) xml;
	Debug.pdb "done";
    | "Perpendicular"  -> 
	let o1 = gattr "point" in
	let o2 = gattr "line" in
	init (new objet_droite_orthogonale o1 o2) xml
    | "PerpendicularBisector"  -> 
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_droite_mediatrice o1 o2) xml 
    | "AngleBisector"  -> 
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	let o3 = gattr "third" in
	init (new objet_droite_bissectrice o1 o2 o3) xml     
    | "Parallel"  -> 
	let o1 = gattr "point" in
	let o2 = gattr "line" in
	init (new objet_droite_parallele o1 o2) xml
    | "IntersectionLineLine"  -> 
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_intersection_droite_droite o1 o2) xml
    | "IntersectionCircleCircle1"  -> 
	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_intersection_cercles o1 o2 1) xml
    | "IntersectionCircleCircle2"  ->
 	let o1 = gattr "first" in
	let o2 = gattr "second" in
	init (new objet_intersection_cercles o1 o2 (-1)) xml
    | "IntersectionCircleLine1"  -> 
	let o1 = gattr "circle" in
	let o2 = gattr "line" in
	init (new objet_intersection_cercle_droite o1 o2 1) xml
    | "IntersectionCircleLine2"  -> 
	let o1 = gattr "circle" in
	let o2 = gattr "line" in
	init (new objet_intersection_cercle_droite o1 o2 (-1)) xml
    | "Transformation" ->
	let o1 = gattr "of" in
	let o2 = gattr "by" in
	  begin
	  match
	    o1#objet_type with
		TYPE_POINT -> 
		  init (new objet_point_transforme o1 o2) xml
	      | TYPE_DROITE ->
		  init (new objet_droite_transforme o1 o2) xml
	      | TYPE_CERCLE ->
		  init (new objet_cercle_transforme o1 o2) xml
	      | TYPE_SEGMENT -> Debug.pdb "TODO segment transforme"
	      | _ -> Debug.pdb "ici";assert false
	  end
    | s -> Debug.pdb s; assert false
  in
  Xml.iter aux xml
    
let process_attributs c =
  Debug.pdb "process_attributs";
  let aux x = match tag x with
      "Attributs" -> process_attribut x
    | s -> Debug.pdb "Attribut unknown";Debug.pdb s;assert false
  in
    Xml.iter aux c;
    Debug.pdb "proccess done"
      
let process_view c =
  Debug.pdb "process_view";
  let att s = of_string (attrib c s) in 
    Repere.set {o={x=att "Origin_x";y= att "Origin_y"};d={x=att "Scale_x";y=att "Scale_y"}}


    
let process_corps c =
  Debug.pdb "process_corps";
  match tag c with
      (*      "CoordinateSystem" as s -> process_inconnu s *)
    | "View" -> process_view c
    | "Hierarchy" -> process_hierarchy c
    | "Attributs"  -> process_attributs c
    | s -> Debug.pdb "body unknown :";Debug.pdb s;assert false
	
let process_file doc =
  match tag doc with "GeoProofDocument" ->
    let v = attrib doc "Version" in
    (
     match v with
       "0.3" | "0.4" -> Xml.iter process_corps doc
     | s -> Printf.eprintf "This GeoProof file version %s can not be dealt with.\n" s
    )
  | s -> Debug.pdb s;assert false

