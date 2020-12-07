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

open Geometric_functions.GeometryFunctions
open Types_de_base
open Fonctions
open Repere
open Options
open Optionsdeconfiguration
open Objets_graphiques
open Objets_ligne
open Construction
open List

class type virtual t_objet_droite =
object
  inherit Objets_ligne.t_objet_ligne
  method dessine :  GDraw.pixmap -> Repere.t -> unit
  method est_proche : Repere.t -> t_coords -> bool
  method objet_type : t_type_objet 
  method output_svg_body : unit -> Xml.xml
end

class virtual objet_droite =
  object (self)
    inherit objet_ligne
	
    method dessine win rep =
      if self#objet_calculable then
	begin
	  let eq = self#equation_droite in
	  let l = droite_inter_cadre rep eq in
	  if length l = 0 
	  then 
	    ()
	  else
	    begin
	      let c1 = hd l
	      and c2 = hd (tl l) in
	      win#line c1.xint c1.yint c2.xint c2.yint;
	    end
	end
	    
    method output_svg_body () =
      if self#objet_calculable then
	begin
	  let eq = self#equation_droite in
	  let l = droite_inter_cadre (Repere.get()) eq in
	  if length l = 0 
	  then 
	    Xml.Element("line",[],[])
	  else
	    begin
	      let c1 = hd l
	      and c2 = hd (tl l) in
	      
              Xml.Element("line",
			  [ (* TODO passer en float et factoriser code *)
			    ("x1",(string_of_int c1.xint));
			    ("x2",(string_of_int c2.xint));
			    ("y1",(string_of_int c1.yint));
			    ("y2",(string_of_int c2.yint))
			  ],[])
	    end
	end
      else
	Xml.Element("line",[],[])

    method est_proche rep c =
      if self#objet_calculable then
	begin
	  let eq = self#equation_droite in
	  distance2_point_droite_pixel rep c eq <= !!pixels_proche
	end
      else false
	  
    method objet_type = TYPE_DROITE
  end

