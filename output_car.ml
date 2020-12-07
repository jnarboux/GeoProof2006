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

let header =  
"<?xml version=\"1.0\" encoding=\"utf-8\"?>"

let car children =
  Element("CaR",[],[
	  Element("Construction",[],
		  [
		   Element("Window",[("x","0.0");("y","0.0");("w","0.0")],[]);
		   Element("Objects",[],children)
		 ])
	])

let to_string () = 
  let body = ref [] in
  let update_body o =  
    body := (o#output_car()) :: !body  
  in
  Construction.iter update_body; 
  header^Xml.to_string_fmt (car !body) 


let to_file s = ()
