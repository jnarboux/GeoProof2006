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

open Objets_graphiques
open Types_de_base
open Document_managment

class studied_fact raw_text gui : t_studied_fact =
  object (self)
    
    val mutable raw_text = raw_text
    val mutable text = ""
    val mutable row = None

    method output_coq () = None
    method output_predicate () = None
    method dependancies =  Evaluation_expressions.dependancies_string raw_text

    method recompute = 
      text <-  Evaluation_expressions.eval_string raw_text;
      Studied_facts.modify_fact gui (unpack row) "i" text "darkgreen" () 
     	
    initializer 
      row <- Some (Studied_facts.add_fact gui "H" text "darkgreen" ());
      self#recompute
	
  end
