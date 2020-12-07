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

(* $Id: objets_cercle.ml,v 1.22 2005/11/24 21:18:52 jnarboux Exp $ *)

open Geometric_functions.GeometryFunctions
open Types_de_base 
open Options
open Optionsdeconfiguration
open Objets_graphiques

(*MG open Fonctions *)
(*MG open Construction *)
(*MG open Repere *)  
(*MG open Xml *)

class type virtual t_objet_cercle =
object
  inherit Objets_graphiques.t_objet_graphique
  method dessine : GDraw.pixmap -> Repere.t -> unit
  method est_proche : Repere.t -> t_coords -> bool
  method objet_type : t_type_objet  
  method output_svg_body : unit -> Xml.xml
  method movable : unit -> bool
end

class virtual objet_cercle =
object (self)
  inherit objet_graphique
    
  method movable () = false
    
  method dessine win rep =
    if self#objet_calculable then
      let eq = self#equation_cercle in
      let center = Repere.papier_vers_ecran rep eq.center
      and rayon = Fonctions.distance_vers_ecran rep eq.radius
      in
	win#arc ~x:(center.xint - rayon) ~y:(center.yint - rayon)
	  ~width: (2 * rayon) ~height: (2 * rayon) ~filled:false
	  ~start:0. ~angle: (360. *. 64.)  ()
	
  (* We use paper coordinates for svg not screen coordinates *)
  method output_svg_body () = 
    if self#objet_calculable then
      let to_string x = Geometric_functions.GeometryFunctions.to_string x !!precision in
      let eq = self#equation_cercle in
      let center = Repere.papier_vers_ecran_t (Repere.get()) eq.center in
      let radius = Fonctions.distance_vers_ecran_t (Repere.get()) eq.radius in
      let children = [] in
      let atts = 
	[("cx",(to_string center.x));
	 ("cy",(to_string center.y));	
	 ("r",(to_string radius))] 
      in
	(Xml.Element("circle",atts,children))
    else
      	(Xml.Element("circle",[],[]))

  method est_proche rep c =
    if self#objet_calculable then
      let eq = self#equation_cercle in
      let p = Repere.ecran_vers_papier c () in
      let dp = distance_point_circle p eq in 
	(Fonctions.distance_vers_ecran rep dp) <= !!pixels_proche
    else false

  method objet_type = TYPE_CERCLE
end

    

  
