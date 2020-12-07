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

let window_about () =
  let windowabout =
    GWindow.window 
     (* ~type_hint:`SPLASHSCREEN *) 
      ~position:`CENTER_ALWAYS 
      ~kind:`POPUP
      ~border_width:3
      ~modal:true ()
  in

  let vbox = GPack.vbox ~spacing:5 ~packing:windowabout#add () in

  let labeltitre =
    GMisc.label 
      ~line_wrap:true 
      ~text:"Welcome to GeoProof !\n
       (https://gna.org/projects/geoproof)"
      ~justify:`CENTER ~packing:vbox#add ()
  in

  let labelcopyright =
    GMisc.label 
      ~text:
      "Copyright (C) 2004-2005\n
      Nicolas Francois\n
      Julien Narboux"
      ~packing:vbox#add ()
  in

  let dessin = GMisc.image  ~packing:vbox#add () in
  let pix = Icons.get_pixbuf Icons.icon_apropos 375 220 in
(*	
  let size = Rsvg.at_max_size 375 220 in
  let pix = Rsvg.render_from_file ~size_cb:size "apropos.svg" in *)

  dessin#set_pixbuf pix;
 
  let label =
    GMisc.label ~line_wrap:true ~text:("Version "^Autoconf.version)
      ~justify:`LEFT ~packing:vbox#add ()
  in

  let button = GButton.button ~label:"Ok" ~packing: vbox#add () in

  ignore (button#connect#clicked ~callback:windowabout#destroy);

  windowabout#show()



