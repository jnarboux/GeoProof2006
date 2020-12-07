(* $Id: objets_point.ml,v 1.25 2005/11/26 17:37:55 jnarboux Exp $ *)

open Geometric_functions.GeometryFunctions
open Types_de_base
open Objets_graphiques
open Options
open Optionsdeconfiguration

(*MG open Fonctions *)
open Repere 
  
class type virtual t_objet_point =
object 
  inherit Objets_graphiques.t_objet_graphique
  method est_proche : Repere.t -> t_coords -> bool
  method dessine :  GDraw.pixmap -> Repere.t -> unit
  method objet_type : t_type_objet  
  method output_svg_body : unit -> Xml.xml
  method movable : unit -> bool
  method defining_points : unit -> (string * string) option
end
	  
class virtual objet_point =
object (self)
  inherit objet_graphique
    
  method movable () = false (* most points are not free *)
    
  method est_proche rep c =
    if self#objet_calculable then
      let u = papier_vers_ecran_t rep self#coordonnees_point in
      let deltax = abs (u.x -! c.x)
      and deltay = abs (u.y -! c.y) 
      in
      (deltax +! deltay) <! (of_int !!pixels_proche)
    else false
	
  method dessine win rep =
    if self#objet_calculable then
      begin
    let dessine_point c =
      match self#objet_style with
	| STYLE_POINT (CROIX) ->
	    win#line (c.xint + 3) (c.yint + 3) (c.xint - 3) (c.yint - 3);
	    win#line (c.xint - 3) (c.yint + 3) (c.xint + 3) (c.yint - 3)
	| STYLE_POINT (ROND) -> 
	    win#arc ~x: (c.xint-4) ~y: (c.yint-4)
	      ~width: 8 ~height: 8 ~filled: false
	      ~start: 0. ~angle: (360. *. 64.)
	      ()
	| STYLE_POINT (CARRE) ->
	    win#rectangle 
	      ~x: (c.xint -3)
	      ~y: (c.yint -3) 
	      ~width: 6 
	      ~height: 6 
	      ~filled: false
	      ()
	| STYLE_POINT (RONDPLEIN) ->
	    win#arc 
	      ~x: (c.xint-4) 
	      ~y: (c.yint-4)
	      ~width: 8 ~height: 8 ~filled: true
	      ~start: 0. ~angle: (360. *. 64.)
	      ()

	| STYLE_POINT (CARREPLEIN) -> 
	    win#rectangle 
	      ~x: (c.xint -3)
	      ~y: (c.yint -3)
	      ~width: 6 
	      ~height: 6 
	      ~filled: true
	      ()
	| _ -> Debug.pdb "le point n'a pas le bon style";()
    in
      dessine_point (papier_vers_ecran rep self#coordonnees_point)
      end
    
	
  (* TODO Nettoyage + factorisation *)
	
	
  method output_svg_body () =
    if self#objet_calculable then
      begin
    let to_str x = to_string x !!precision in
    let dis = of_int 3 in
    let c = papier_vers_ecran_t (Repere.get()) self#coordonnees_point in
      match self#objet_style with
	| STYLE_POINT (CROIX) ->
	    let cx = to_str c.x in
	    let cy = to_str c.y in
	    let cx3 = to_str (c.x -! dis) in
	    let cy1 = to_str (c.y -! one) in
	    let cx1 = to_str (c.x -! one) in
	    let cy3 = to_str (c.y -! dis) in
	      
	      Xml.Element ("g",
			   [("transform", "rotate(33 "^cx^" "^cy^")")],
			   [
			     Xml.Element ("rect",
					  [("x",cx3);
					   ("y",cy1);
					   ("width","6");
					   ("height","2")
					  ],[]); 
			     Xml.Element ("rect",
					  [("x",cx1);
					   ("y",cy3);
					   ("width","2");
					   ("height","6")
					  ],[])
			   ])
	| STYLE_POINT (ROND) ->
	    Xml.Element ("circle",
			 [("cx",(to_str c.x));
			  ("cy",(to_str c.y));
			  ("r","3")
			 ],[])
	      
	| STYLE_POINT (RONDPLEIN) -> 
	    Xml.Element ("circle",
			 [("cx",(to_str c.x));
			  ("cy",(to_str c.y));
			  ("r","3")
			 ],[])
	      
	| STYLE_POINT (CARREPLEIN)  -> 
	    Xml.Element ("rect",
			 [("x",(to_str (c.x -! dis)));
			  ("y",(to_str (c.y -! dis)));
			  ("width","6");
			  ("height","6")
			 ],[])
	| STYLE_POINT (CARRE) ->
	    Xml.Element ("rect",
			 [("x",(to_str (c.x -! dis)));
			  ("y",(to_str (c.y -! dis)));
			  ("width","6");
			  ("height","6")
			 ],[])
	| _ ->  Debug.pdb "le point n'a pas le bon style";
	    Xml.Element ("rect",[("x","0");("y","0")],[])
      end
	else
      Xml.Element ("text",[],[])
	      
  method objet_type = TYPE_POINT

  method defining_points () = None
end
  
    
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
  
