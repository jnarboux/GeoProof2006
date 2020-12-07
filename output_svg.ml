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

open Geometric_functions 
open GeometryFunctions

let header =  
"<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20001102//EN\" \"http://www.w3.org/TR/2000/CR-SVG-20001102/DTD/svg-20001102.dtd\">"

let svg children = Xml.Element("svg",
		   [("xmlns","http://www.w3.org/2000/svg");
		    ("width",(to_string !Repere.screen_width 5));
		    ("height",(to_string !Repere.screen_height 5));
		    ("id","testexportsvg")],
			       children
			      )

let to_string () = 
  let body = ref [] in
  let update_body o = 
    if o#objet_visible then
      body := (o#output_svg()) :: !body  
  in
  Construction.iter update_body; 
  header^Xml.to_string_fmt (svg !body) 


let to_file file =
  let aux c =   Printf.fprintf c "%s\n" (to_string ()) in
  Output_fonctions.write_file file aux

