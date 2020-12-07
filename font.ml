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

(* $Id: font.ml,v 1.6 2005/11/13 13:03:40 jnarboux Exp $ *)

let font_choosed = "sans 12"

let draw (pixmap : GDraw.pixmap) text x y =
  let toto = GMisc.label ~text:"toto" () in 
(* create a label but this  could be any object *)
  let context = toto#misc#create_pango_context in
  context#set_font_by_name font_choosed;
  let layout = context#create_layout in
  Pango.Layout.set_text layout text;
  pixmap#put_layout ~x ~y layout


let get_pixel_size text = 
  let toto = GMisc.label ~text:"toto" () in 
    (* create a label but this  could be any object *)
  let context = toto#misc#create_pango_context in
    context#set_font_by_name font_choosed;
    let layout = context#create_layout in
      Pango.Layout.set_text layout text;
      Pango.Layout.get_pixel_size layout
	

