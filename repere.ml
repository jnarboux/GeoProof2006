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

(* $Id: repere.ml,v 1.13 2005/06/20 09:03:57 jnarboux Exp $ *)

open Types_de_base
open Geometric_functions (*.GeometryFunctions *)
open GeometryFunctions

(* TODO à  patcher pour initialiser à la taille de la fenêtre *)
let screen_width = ref (of_int 1000)
let screen_height = ref (of_int 600)  


let change_screen_width x = screen_width := of_int x
let change_screen_height x = screen_height := of_int x


type t =
    {
      mutable o : t_coords;
      mutable d : t_coords;
    }
      
let repere = ref  
  {
    o = {x = !screen_width /! two;
	 y = !screen_height /! two};
    d = {x=of_int 50;y=of_int 50}
  } 
  
  
let get() = !repere

let set r = repere := r
  
(* Fonctions de transformation des systèmes de coordonnées *)
let papier_vers_ecran rep c =
  { xint = to_int (c.x *! rep.d.x +! rep.o.x);
    yint = to_int (c.y *! rep.d.y +! rep.o.y)}
    
let papier_vers_ecran_t rep c =
  { x = (c.x *! rep.d.x +! rep.o.x);
    y = (c.y *! rep.d.y +! rep.o.y)}

let ecran_vers_papier c () =
  { x = (c.x -! !repere.o.x) /! !repere.d.x;
    y = (c.y -! !repere.o.y) /! !repere.d.y}
 
let origine_ecran_t rep = 
  papier_vers_ecran_t rep {x=of_int 0;y=of_int 0}
    
let origine_ecran_int rep =
  papier_vers_ecran rep {x=of_int 0;y=of_int 0}

let _ = Random.self_init()

let random_point() = 
  {
   x= of_int (Random.int ((to_int (abs !repere.o.x)) +1));
   y= of_int (Random.int ((to_int (abs !repere.o.y)) +1))
 }

let upper_left_corner() =
  ecran_vers_papier {x=of_int 0; y= !screen_height} ()

let lower_left_corner() =
  ecran_vers_papier {x=of_int 0; y= of_int 0} ()

let lower_right_corner() =
  ecran_vers_papier {x= !screen_width; y=of_int 0} ()
 
let upper_right_corner() =
   ecran_vers_papier {x= !screen_width; y= !screen_height} ()

let x_min () = 
  (lower_left_corner ()).x
let x_max () = 
  (upper_right_corner()).x
let y_min () = 
  (lower_left_corner()).y
let y_max () = 
  (upper_right_corner()).y

let zoomAvant () =
  let sqrt_sqrt_2 = sqrt (sqrt two) in
  let rep =
    {o = 
	{
	  x= (!repere.o.x -! !screen_width /! two)
	    *! sqrt_sqrt_2 +! !screen_width /! two;
	  y = (!repere.o.y -! !screen_height /! two) *!
	    sqrt_sqrt_2 +! !screen_height /! two
	};
     d = 
	{
	  x = !repere.d.x *! sqrt_sqrt_2;
	  y = !repere.d.y *!sqrt_sqrt_2
	}
    }
  in
    repere := rep


let zoomArriere () =
  let one_over_sqrt_sqrt_2 = one /! (sqrt (sqrt two)) in
  let rep =
    {
      o = 
	{
	  x = (!repere.o.x -! !screen_width /! two) *!
	    one_over_sqrt_sqrt_2 +! !screen_width /! two;
	  y = (!repere.o.y -! !screen_height /! two) *!
	    one_over_sqrt_sqrt_2 +! !screen_height /! two
	};
      d = 
	{
	  x = !repere.d.x *! one_over_sqrt_sqrt_2;
	  y = !repere.d.y *! one_over_sqrt_sqrt_2
	}
    }
  in
    repere := rep

let zoomAjuste minx miny maxx maxy =
  let dx = maxx -! minx in
  let dy = maxy -! miny in
  let ddx = !screen_width /! dx in
  let ddy = !screen_height /! dy in
  let d = min ddx ddy in
  
  let rep =
    {o = { x=  neg minx *! d;
	   y = neg miny *! d;};
     d = {x= d;y=d}
    }
  in
    repere := rep  
      
let deplaceHaut () =
  let rep =
    {o = {x = (!repere.o.x);
	  y = (!repere.o.y +! !screen_height /! (of_int 8))};
     d = !repere.d;
    }
  in
    repere := rep
      
      
let deplaceBas () =
  let rep =
    {
      o = {x =  (!repere.o.x);
	   y = (!repere.o.y -! !screen_height /! (of_int 8))};
      d = !repere.d
    }
  in
  repere := rep

let deplaceGauche () =
  let rep =
    {o = {x = (!repere.o.x -! !screen_width /! (of_int 8));
	  y = (!repere.o.y )};
     d = !repere.d}
  in
    repere := rep
      
let deplaceDroite () =
  let rep =
    {o = {x = (!repere.o.x +! !screen_width /! (of_int 8));
	  y = (!repere.o.y )};
     d = !repere.d}
  in
  repere := rep

let move_paper p1 p2 () =
  let rep =
    {o = v_plus !repere.o (v_minus p2 p1);
     d = !repere.d}
  in
    repere := rep
      
