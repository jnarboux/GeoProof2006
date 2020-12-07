(* $I$ *)

type t_coords_int = {mutable xint : int; mutable yint : int}

type coloring = 
    {
      color : int*int*int;  
      alpha : int
    }
      
type point_shape =
  | CROSS
  | DISC
  | CIRCLE
  | BOX
  | SQUARE
  | PLUS

type line_shape =
  | DASHED of int list
  | SOLID

type width = int

type size = int

type pattern =
  | SOLID
  | DASHED of int list*int (*TODO*)

type arrow_shape =
  | ARROW1
  | ARROW2

type font = string (* TODO *)

type text_style =
    {
      font : font;
      color : coloring;
      framed : bool	
    }

type fill_style =
    { 
      color : coloring;
      pattern : pattern
    } 

type point_style =
    {
      shape : point_shape;
      color : coloring;
      width : width;
      size : size 
    }

type line_style =
    {
      shape : line_shape;
      color : coloring;
      width : width 
    }

type vector_style =
    {  
      line_style : line_style;
      arrow_shape : arrow_shape	
    }

type axes_style =
    { 
      grid_shown : bool;
      scale_shown : bool;
      scale_style : text_style;
      grid_style : line_style;
      axes_style : line_style
    }
      
type polygon_style =
    {
      fill_style : fill_style;
      border_style : line_style
    }
      
type mark_shape =
  | LINE of int
  | CROSS of int 
      
type mark_style =
    { 
      shape : mark_shape;
      style : line_style;
      size : size
    }

type style_point =
  | CROIX
  | ROND
  | CARRE
  | RONDPLEIN
  | CARREPLEIN

type style_droite =
  | PLEINE
  | POINTILLEE

type style_texte =
  { 
    texte : string
  }

type style_repere =
    { grid_shown : bool }

type style_mark =
  | LINE of int
  | CROSS of int 

type unité_distance = 
  | CMS
  | POUCES
  | POINTS

type unité_angle = 
  | DEGRES
  | RADIANS

type formal_langage =
  | Narboux
  | Guilhot

type studied_facts_cols = 
    {
      name:string GTree.column;
      descr:string GTree.column; 
      color:string GTree.column;
    }

type studied_facts_gui =
    { 
      model : GTree.list_store;
      cols : studied_facts_cols
    }

    
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
  
