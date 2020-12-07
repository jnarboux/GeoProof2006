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

(* $Id: objets_texte.ml,v 1.32 2005/11/28 18:51:08 jnarboux Exp $ *)

open Geometric_functions.GeometryFunctions
open Objets_graphiques
open Optionsdeconfiguration
open Options
open Repere
open Types_de_base
open Fonctions

class type virtual t_objet_texte =
  object
    inherit Objets_graphiques.t_objet_graphique
    method dessine :  GDraw.pixmap -> Repere.t -> unit
    method est_proche : Repere.t -> t_coords -> bool
    method objet_type : t_type_objet 
    method output_svg_body : unit -> Xml.xml
    method output_coq_body : unit -> Fol.fol Formulas.formula option
    method output_coq_tactic : unit -> string option

    method movable : unit -> bool
    method defining_points : unit -> (string * string) option
      
    method private get_text : unit -> string  
      (* contains the text with possibly some expressions  between #*)
    method private set_text : string -> unit
    method private get_text_value : unit -> string 
      (* contains the evaluation of the text *) 
    method private set_text_value : string -> unit
  end
      
class virtual objet_texte =
  object (self)
    inherit objet_graphique
      
    val mutable texte_value = "ulabel"
    val mutable size = (0,0) (* size of the text area in pixels *)

    method movable () = true

    method private get_text () = 
      match self#objet_style with
	STYLE_TEXTE t -> t.texte
      | _ -> "uninitialized text" (* assert false *)
	    
    method private set_text text = 
      match self#objet_style with
	  STYLE_TEXTE t -> 
	    self#change_style (STYLE_TEXTE {t with texte = text})
	| _ -> 
	    self#change_style (STYLE_TEXTE {texte = text})

    method private get_text_value () = texte_value
    method private set_text_value s =     
      size <- Font.get_pixel_size s; 
      texte_value <- s

    method dessine win rep =
      if self#objet_calculable then
	let dessine_texte c =
	  Font.draw win (self#get_text_value())  c.xint c.yint
	in
	dessine_texte (papier_vers_ecran rep self#coordonnees_point)
	
    method est_proche rep c =
      if self#objet_calculable then
	let u = papier_vers_ecran_t rep self#coordonnees_point in  
	let demie_largeur_label = (of_int (fst size)) /! two in
	let demie_hauteur_label = (of_int (snd size)) /! two in
	let centre_label_x = u.x +! demie_largeur_label in
	let centre_label_y = u.y +! demie_hauteur_label in
	let deltax = abs (centre_label_x -! c.x) 
	and deltay = abs (centre_label_y -! c.y) in
	
	deltax <! demie_largeur_label +! (of_int !!pixels_proche) && 
	deltay <! demie_hauteur_label +! (of_int !!pixels_proche)
	else
	false

    method objet_type = TYPE_TEXTE

    method output_svg_body () = 
      if self#objet_calculable then
	let c = (papier_vers_ecran_t (Repere.get()) self#coordonnees_point) in
	Xml.Element("text",[
		    ("x",(to_string c.x !!precision));
		    ("y",(to_string c.y !!precision));
		  ],[Xml.PCData (self#get_text_value())])
      else
	Xml.Element("text",[],[])

    method defining_points () = None

    method virtual recalcule : unit
    method virtual depend_de : t_objet_graphique -> bool
    method virtual dependance : t_objet_graphique list
    method virtual output_coq_body : unit -> Fol.fol Formulas.formula option
  end
