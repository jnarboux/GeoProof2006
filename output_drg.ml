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
open Repere
open Geometric_functions.GeometryFunctions

let view () = 
  let tos x = Geometric_functions.GeometryFunctions.to_string x 5 in
  let r = Repere.get() in 
  let xo = tos r.o.x in
  let yo =  tos r.o.y in
  let xd = tos r.d.x in
  let yd =  tos r.d.y in
    Element("View",[
	      ("Origin_x",xo);
	      ("Origin_y",yo);
	      ("Scale_x",xd);
	      ("Scale_y",yd)],[])      
      
      
let aux c =
  let xml =   
    Element("GeoProofDocument",       
	    [("Version",Autoconf.version)],
	    [view();
	     Element("Hierarchy",[],
		     (Construction.map (fun o -> o#output_drg_body ())));
	     Element("Attributs",[],
		     (Construction.map (fun o -> o#output_drg_attributs ())))
	    ]
	   )
  in 
    Printf.fprintf c "%s\n" (to_string_fmt xml)
      
(*
  let to_string = Output_fonctions.to_string aux
  
  let to_file  ?(overwrite=false) s = 
  Output_fonctions.to_file ~overwrite aux s
*)

let to_file ?(overwrite=false) s = 
  Output_fonctions.write_file s ~overwrite aux
