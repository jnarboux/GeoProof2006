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

(* $Id: input_xml.ml,v 1.15 2005/06/20 09:03:57 jnarboux Exp $ *)

open Xml
open Printf
open I18n
open Input_exceptions  

let show_error_message file s = 
  GToolbox.message_box 
    ~title:((i18n "Error opening file ")^file) 
    ((i18n "Error opening file ")^file^" :\n"^s)

let entree_xml file =  
  try 
    let doc = parse_file file in
      begin
	match 
	  tag doc 
	with 
	    "CaR" ->
	      Input_xml_CaR.process_file doc; Construction.zoom_ajuste ()
	  | "KigDocument" ->
	      Input_xml_Kig.process_file doc; Construction.zoom_ajuste ()
	  | "ConstantType" ->
	      Input_xml_Coq.process_file doc;  
	  | "ConstructionSequence" ->
	      Input_xml_constructive_theorems.process_file doc;  Construction.zoom_ajuste () 
	  | "GeoProofDocument" ->
	      Input_xml_Drg.process_file doc;
	  | s ->
	      show_error_message file (s^": "^(i18n "File type unknown."))
      end
  with
      Error (m,p) ->
	let error = error_msg m in  
	let line = string_of_int (line p) in
	  show_error_message file ("Line "^line^" : "^error)
    | Undefined_Object o ->
	show_error_message file (o^" : "^(i18n "Object undefined"))
    | e ->   
	show_error_message file (i18n "Unknown error")
