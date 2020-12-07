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

open Geometric_functions.GeometryFunctions
open I18n


let window () =  
  let c = ref (Some {x=of_int 0;y=of_int 0}) in
  let w = GWindow.dialog ~title:(i18n "Input coordinates") 
    ~allow_shrink:false ~allow_grow:false ~modal:true () in
  ignore (w#connect#destroy ~callback: GMain.Main.quit);
  let expl = GMisc.label
    ~xalign:0.1
    ~text:(i18n "Please input the coordinates for the new point :")  
    ~packing:(w#vbox#pack ~expand:false)
    () in

  let hbox = GPack.hbox ~packing:w#vbox#pack () in
  let xl = GMisc.label
    ~xalign:0.1
    ~text:(i18n "x =")  
    ~packing:(hbox#pack ~expand:false)
    () in
  let x = GEdit.entry ~packing:hbox#pack () in
  let yl = GMisc.label
    ~xalign:0.1
    ~text:(i18n "y =")  
    ~packing:(hbox#pack ~expand:false)
    () in
  let y = GEdit.entry ~packing:hbox#pack () in

  let hboxbuttons = GPack.hbox ~packing:w#action_area#pack () in

  let button_cancel = GButton.button
    ~label:(i18n "Cancel")
    ~packing:hboxbuttons#add () in
    
    ignore (button_cancel#connect#clicked  ~callback:(fun () -> c:=None; w#destroy()));
    
  let button_ok = GButton.button
      ~label:(i18n "Ok")
      ~packing:hboxbuttons#add () in

  button_ok#misc#set_sensitive false;

  let coords () =
    ignore (float_of_string x#text);
    ignore (float_of_string y#text);
    Some {x=of_string x#text;y=of_string y#text}
  in
  
  let on_change () =
    try
      c := coords();
      button_ok#misc#set_sensitive true
    with 
      _ -> 
	button_ok#misc#set_sensitive false
  in
  
  ignore(x#connect#changed ~callback:on_change);
  ignore(y#connect#changed ~callback:on_change);
  
  ignore (button_ok#connect#clicked  ~callback:(fun () -> w#destroy()));
  
  x#misc#grab_focus ();
  w#set_position `CENTER;
  w#show (); 
  GMain.Main.main ();
  !c

  

