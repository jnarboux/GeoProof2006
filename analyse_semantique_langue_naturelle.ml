(* $Id: analyse_semantique_langue_naturelle.ml,v 1.16 2005/10/29 02:38:59 eul_bofo Exp $ *)

open I18n
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
open Objets_cercle_diametre
open Objets_point_transforme
open Objets_droite_transforme
open Objets_cercle_transforme
open Objets_repere
open Fonctions
open Options
open Optionsdeconfiguration

exception Wrong_number_declaration of (int * string)
exception Object_does_not_exist of string
exception Object_defined_twice of string
exception Intersection_with_point of string
exception Double_intersection_required of string
exception Simple_intersection_required of (string * string)
exception X_found_Y_expected of string*string*string (* name found expected *)
exception Dash_unexpected

type attribut = {couleur : GDraw.color; pointille : bool; visi : bool; epaisseur : int; numero_couche : int; style_point : style_point }

let init o name att =
  o#nomme name;
  o#change_couleur att.couleur;
  if att.pointille then
    begin
      match o#objet_type with
	TYPE_DROITE | TYPE_SEGMENT | TYPE_CERCLE ->
	  o#change_style (STYLE_DROITE POINTILLEE) 
      | _ -> raise Dash_unexpected
    end;
  begin
    match o#objet_type with
      TYPE_POINT -> o#change_style (STYLE_POINT att.style_point)
    | _ -> ()
  end;
  o#change_epaisseur att.epaisseur; 
  o#change_visibilite att.visi;
  o#change_couche att.numero_couche;
  Document_managment.add_object o (*;
  o*)

let repere name att =
  init  (new objet_repere) name att

let point name att = 
  init (new objet_point_clique (random_point())) name att
    
let multiple_point l att = List.hd (List.map (fun f -> f att) l)

let get_object s = 
  try get_object s with
    Not_found -> raise (Object_does_not_exist s)
  | MoreThanOne -> raise (Object_defined_twice s)

let string_of_type t = match t with 
| TYPE_DROITE -> (i18n "a line")
| TYPE_CERCLE -> (i18n "a circle")
| TYPE_POINT -> (i18n "a point")
| TYPE_SEGMENT -> (i18n "a segment")
| TYPE_VECTEUR ->  (i18n "a vector")
| TYPE_TEXTE -> (i18n "a label")
| TYPE_REPERE -> (i18n "a grid")
| TYPE_MARK ->  (i18n "a mark")
      
let point_on name s att =
  let o = get_object s in
  match o#objet_type with 
  | TYPE_DROITE -> init (new objet_point_sur_droite o (random_point())) name att
  | TYPE_CERCLE -> init (new objet_point_sur_cercle o (random_point())) name att
  | t ->  raise (X_found_Y_expected(s,(string_of_type t),(i18n "a line or a circle")))

let intersection name s1 s2 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
  match (o1#objet_type,o2#objet_type) with
  | (TYPE_POINT,_) -> raise (Intersection_with_point s1)
  | (_,TYPE_POINT) -> raise (Intersection_with_point s2)
  | ((TYPE_DROITE|TYPE_SEGMENT),(TYPE_DROITE|TYPE_SEGMENT)) -> init (new objet_intersection_droite_droite o1 o2) name att
  | (TYPE_CERCLE,_) -> raise (Double_intersection_required s1)
  | (_,TYPE_CERCLE) -> raise (Double_intersection_required s2)
  | ((TYPE_DROITE|TYPE_SEGMENT),t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a line or a circle")))
  | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a line or a circle")))

let double_intersection name1 name2 s1 s2 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
  match (o1#objet_type,o2#objet_type) with
  | (TYPE_POINT,_) -> raise (Intersection_with_point s1)
  | (_,TYPE_POINT) -> raise (Intersection_with_point s2)
  | ((TYPE_DROITE|TYPE_SEGMENT),(TYPE_DROITE|TYPE_SEGMENT)) -> raise (Simple_intersection_required (s1,s2))
  | ((TYPE_DROITE|TYPE_SEGMENT),TYPE_CERCLE) ->  
      init (new objet_intersection_cercle_droite o2 o1 (-1)) name1 att;
      init (new objet_intersection_cercle_droite o2 o1 1) name2 att
  | (TYPE_CERCLE,(TYPE_DROITE|TYPE_SEGMENT)) -> 
      init (new objet_intersection_cercle_droite o1 o2 (-1)) name1 att;
      init (new objet_intersection_cercle_droite o1 o2 1) name2 att
  | (TYPE_CERCLE,TYPE_CERCLE) -> 
      init (new objet_intersection_cercles o1 o2 (-1)) name1 att;
      init (new objet_intersection_cercles o1 o2 1) name2 att
  | ((TYPE_DROITE|TYPE_SEGMENT),t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a line or a circle")))
  | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a line or a circle")))


let middle name s1 s2 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
  match (o1#objet_type,o2#objet_type) with
  | (TYPE_POINT,TYPE_POINT) -> 
      init (new objet_point_milieu o1 o2) name att
  | (TYPE_POINT,t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
  | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a point")))
      
let middle2 name s att =
  let o = get_object s in
    match o#objet_type with
      | TYPE_SEGMENT  -> assert false (* TODO *)
      | t -> raise (X_found_Y_expected(s,(string_of_type t),(i18n "a segment")))
	  
let any_line name att = assert false (* TODO *)
  
let line name s1 s2 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
    match (o1#objet_type,o2#objet_type) with
      | (TYPE_POINT,TYPE_POINT) -> init (new objet_droite_deux_points o1 o2) name att
      | (TYPE_POINT,t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
      | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a point")))
	  
let segment name s1 s2 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
    match (o1#objet_type,o2#objet_type) with
      | (TYPE_POINT,TYPE_POINT) ->  init (new objet_segment_deux_points o1 o2) name att
      | (TYPE_POINT,t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
      | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a point")))

let vecteur name s1 s2 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
    match (o1#objet_type,o2#objet_type) with
      | (TYPE_POINT,TYPE_POINT) ->  init (new objet_vecteur_deux_points o1 o2) name att
      | (TYPE_POINT,t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
      | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a point")))
	  
let parallel name s1 s2 att =
  let d = get_object s1 in
  let p = get_object s2 in
    match (d#objet_type,p#objet_type) with
      | ((TYPE_DROITE|TYPE_SEGMENT),TYPE_POINT) -> init (new objet_droite_parallele p d) name att
      | ((TYPE_DROITE|TYPE_SEGMENT),t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
      | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a line")))

(*
let parallel_seg name s1 s2 s3 s4 att = ()
  let o1 = get_object s1 in
  let o2 = get_object s2 in
  let o3 = get_object s3 in
  let o4 = get_object s4 in
  match (o1#objet_type,o2#objet_type,o3#objet_type,o4#objet_type) with
  | (TYPE_DROITE,TYPE_POINT,TYPE_POINT,TYPE_POINT) -> assert false (* TODO *)
  | (TYPE_DROITE,t1,t2,t3) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
  | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a line")))


  | (TYPE_POINT,_,_,_) -> raise (Point_found_line_expected s1)
  | (TYPE_DROITE,TYPE_DROITE,_,_) -> raise (Line_found_point_expected s2)
  | (TYPE_DROITE,TYPE_CERCLE,_,_) -> raise (Circle_found_point_expected s2)
  | (TYPE_CERCLE,_,_,_) -> raise (Circle_found_line_expected s1)
  | (TYPE_DROITE,TYPE_POINT,TYPE_DROITE,_) -> raise (Line_found_point_expected s3)
  | (TYPE_DROITE,TYPE_POINT,TYPE_CERCLE,_)  -> raise (Circle_found_point_expected s3)
  | (TYPE_DROITE,TYPE_POINT,TYPE_POINT,TYPE_DROITE) -> raise (Line_found_point_expected s4)
  | (TYPE_DROITE,TYPE_POINT,TYPE_POINT,TYPE_CERCLE) -> raise (Circle_found_point_expected s4)
*)

let perpendicular name s1 s2 att =
  let d = get_object s1 in
  let p = get_object s2 in
  match (d#objet_type,p#objet_type) with
  | ((TYPE_DROITE|TYPE_SEGMENT),TYPE_POINT) -> init (new objet_droite_orthogonale p d) name att
  | ((TYPE_DROITE|TYPE_SEGMENT),t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
  | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a line")))

(*
let perpendicular_seg name s1 s2 s3 s4 att = ()
  let o1 = get_object s1 in
  let o2 = get_object s2 in
  let o3 = get_object s3 in
  let o4 = get_object s4 in
  match (o1#objet_type,o2#objet_type,o3#objet_type,o4#objet_type) with
  | (TYPE_DROITE,TYPE_POINT,TYPE_POINT,TYPE_POINT) -> assert false (* TODO *) 
  | (TYPE_POINT,_,_,_) -> raise (Point_found_line_expected s1)
  | (TYPE_DROITE,TYPE_DROITE,_,_) -> raise (Line_found_point_expected s2)
  | (TYPE_DROITE,TYPE_CERCLE,_,_) -> raise (Circle_found_point_expected s2)
  | (TYPE_CERCLE,_,_,_) -> raise (Circle_found_line_expected s1)
  | (TYPE_DROITE,TYPE_POINT,TYPE_DROITE,_) -> raise (Line_found_point_expected s3)
  | (TYPE_DROITE,TYPE_POINT,TYPE_CERCLE,_)  -> raise (Circle_found_point_expected s3)
  | (TYPE_DROITE,TYPE_POINT,TYPE_POINT,TYPE_DROITE) -> raise (Line_found_point_expected s4)
  | (TYPE_DROITE,TYPE_POINT,TYPE_POINT,TYPE_CERCLE) -> raise (Circle_found_point_expected s4)
*)

let perpendicular_bissector name s1 s2 att =
  let p1 = get_object s1 in
  let p2 = get_object s2 in
  match (p1#objet_type,p2#objet_type) with
  | (TYPE_POINT,TYPE_POINT) -> init (new objet_droite_mediatrice p1 p2) name att
  | (TYPE_POINT,t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
  | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a point")))
	
let bissecting name s1 s2 s3 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
  let o3 = get_object s3 in
  match (o1#objet_type,o2#objet_type,o3#objet_type) with
  | (TYPE_POINT,TYPE_POINT,TYPE_POINT) ->  init (new objet_droite_bissectrice o1 o2 o3) name att
  | (TYPE_POINT,TYPE_POINT,t) -> raise (X_found_Y_expected(s3,(string_of_type t),(i18n "a point")))
  | (TYPE_POINT,t,_) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
  | (t,_,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a point")))

let circle name s1 s2 att =
  let o1 = get_object s1 in
  let o2 = get_object s2 in
  match (o1#objet_type,o2#objet_type) with
  | (TYPE_POINT,TYPE_POINT) -> init (new objet_cercle_centre_point o1 o2) name att
  | (TYPE_POINT,t) -> raise (X_found_Y_expected(s2,(string_of_type t),(i18n "a point")))
  | (t,_) ->  raise (X_found_Y_expected(s1,(string_of_type t),(i18n "a point")))

    
(*

 GeoProof, an interactive geometry tool writen in OCaml.                   
 Copyright (C) 2004 Nicolas François et Julien Narboux                      
                                                                            
 This program is free software; you can redistribute it and/or              
 modify it under the terms of the GNU General Public License                
 as published by the Free Software Foundation; either version 2             
 of the License, or (at your option) any later version.                     
                                                                            
 This program is distributed in the hope that it will be useful,            
 but WITHOUT ANY WARRANTY; without even the implied warranty of             
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              
 GNU General Public License for more details.                               
                                                                            
 You should have received a copy of the GNU General Public License          
 along with this program; if not, write to the Free Software                
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)
  
