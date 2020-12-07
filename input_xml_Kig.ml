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
open Optionsdeconfiguration
open Geometric_functions.GeometryFunctions

let traite_inconnu s = 
  Printf.eprintf "Le tag %s n'est pas géré.\n" s

   
let color_of_string s =
  match s with
    "#0000ff" -> blue
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
  | "DashLIne"
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
       

let premier xml = 
  attrib (List.hd (children xml)) "id"

let second xml =
  attrib (List.hd (List.tl (children xml))) "id"

let troisieme xml =
  attrib (List.hd (List.tl (List.tl (children xml)))) "id"


let traite_attributs xdata =
  let attrib s = attrib xdata s in
  let o = get_object (attrib "object") in
  let couleur =
    try 
      color_of_string (attrib "color")
    with _ -> black
  in
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
	    line_style_of_string (attrib "style")
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
    | TYPE_TEXTE -> BLOP
  in
  let epaisseur =
     try 
       let e = int_of_string (attrib "width") in
       if e <= 0 then 2 else e
     with
       _ -> 2
  in  
  o#change_couleur couleur;
  o#change_visibilite visi;
  o#change_style style;
  o#change_epaisseur epaisseur

let init o xml = 
  let nom = 
    try 
      attrib xml "id"
    with _ -> "default_name"
  in
  o#nomme nom;
  Document_managment.add_object o

let doubledatas = Hashtbl.create 17
let intdatas = Hashtbl.create 17
let pointdatas = Hashtbl.create 17
let stringdatas = Hashtbl.create 17
 
let traite_data xml =
  match attrib xml "type" with     
    "double" ->
      let key = attrib xml "id" in 
      Hashtbl.add doubledatas key (of_string (pcdata (List.hd (children xml))))
  | "int" -> 
      let key = attrib xml "id" in 
      Hashtbl.add intdatas key (int_of_string (pcdata (List.hd (children xml))))
  | "string" ->
      let key = attrib xml "id" in 
      Hashtbl.add stringdatas key (pcdata (List.hd (children xml)))
  | "point" -> 
      let key = attrib xml "id" in 
      let x = of_string (pcdata (List.hd (children (List.hd (children xml))))) in
      let y = of_string (pcdata (List.hd (children (List.hd (List.tl (children xml)))))) in
      Hashtbl.add pointdatas key {x=x;y=y}
  | s -> Debug.pdb s;assert false 
  
  
let traite_objet xml =
  let s =  attrib xml "type" in
  match s with     
    "Vector" -> 
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_vecteur_deux_points o1 o2) xml
  | "CircleBCP" -> 
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_cercle_centre_point o1 o2) xml
  | "CircleBPR" -> traite_inconnu s
  | "CircleBTP" -> 
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      let o3 = get_object (troisieme xml) in
      init (new objet_cercle_trois_points o1 o2 o3) xml
  | "FixedPoint" -> 
      let x = Hashtbl.find doubledatas (premier xml) in
      let y = Hashtbl.find doubledatas (second xml) in
      init (new objet_point_clique {x=x;y=y}) xml
  | "PythonCompileType" -> traite_inconnu s
  | "PythonExecuteType" -> traite_inconnu s
  | "LineByVector" -> traite_inconnu s
  | "VectorEquality" -> traite_inconnu s
  | "ConstrainedPoint" -> traite_inconnu s
  | "Locus" ->  traite_inconnu s
  | "LineAB" -> 
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_droite_deux_points o1 o2) xml
  | "RayAB" -> 
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_droite_deux_points o1 o2) xml
  | "SegmentAB" ->
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_segment_deux_points o1 o2) xml
  | "Similitude" -> traite_inconnu s
  | "Label" -> 
(*
      let x = Hashtbl.find doubledatas (premier xml) in
*)
      let p = Hashtbl.find pointdatas (second xml) in
      let t = Hashtbl.find stringdatas (troisieme xml) in

	init (new objet_texte_clique p t) xml
  | "LinePerpend" -> 
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_droite_orthogonale o2 o1) xml
  | "LineParallel" ->
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_droite_parallele o2 o1) xml
  | "ConicLineIntersection" ->  traite_inconnu s
  | "LineLineIntersection" -> 
      let o1 =  get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_intersection_droite_droite o2 o1) xml
  | "CircleCircleIntersection" -> 
      let o1 = get_object (premier xml) in
      let o2 = get_object (second xml) in
      init (new objet_intersection_cercles o1 o2 1) xml
  | "VectorSum" -> traite_inconnu s
  | "Copy" -> traite_inconnu s
  | s -> Debug.pdb s; assert false


let traite_hierarchy constr =
  let aux x = match tag x with
    "Data" -> traite_data x
  | "Object" -> traite_objet x
  | "Property" as s -> traite_inconnu s
  | s -> Debug.pdb s;assert false
  in
  Xml.iter aux constr

let traite_view c =
    let aux x = match tag x with
    "Draw" -> traite_attributs x
  | s -> Debug.pdb s;assert false
  in
  Xml.iter aux c
 
let traite_corps c =
  match tag c with
    "CoordinateSystem" as s -> traite_inconnu s
  | "Hierarchy" -> traite_hierarchy c
  | "View"  -> traite_view c
  | s -> Debug.pdb s;assert false


let process_file doc =
  match tag doc with "KigDocument" ->
    let v = attrib doc "Version" in
    (
     match v with
       "0.7.1" | "0.9.1" -> Xml.iter traite_corps doc
     | s -> Printf.eprintf "This Kig file version %s can not be dealt with.\n" s
    )
  | s -> Debug.pdb s;assert false

