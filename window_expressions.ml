(*

 GeoProof, an interactive geometry tool writen in OCaml.                   
 Copyright (C) 2005 Julien Narboux                      
                                                                            
 This program is free software; you can redistribute it and/or              
 modify it under the terms of the GNU General Public License                
 as published by the Free Software Foundation; either version 2             
 of the License, or (at your option) any later version.                     
                                                                            
 This program is distributed in the hope that it will be useful,            
 but WITHOUT ANY WARRANTY; without even the implied warranty of             
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              
 GNU General Public License for more details.                               
                                                                            
 You should have received a copy of the GNU General Public License          
 along with this program; if not, write to the Free Software                
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)

open Types_de_base
open Objets_graphiques
open I18n

let edit_expression x redraw_fun () =
  let current_text = 
    match x#objet_style with
	STYLE_TEXTE {texte=e} -> e
      | _ -> x#objet_nom 
  in

  let w = GWindow.window ~title:(i18n "Input text and/or expressions") 
      ~allow_shrink:false ~allow_grow:true () in
  let vbox = GPack.vbox ~packing:w#add () in
 
  let main = GBin.frame
    ~border_width:6
    ~label:(i18n "Input the text you want to display.")  
    ~packing:(vbox#pack ~expand:true)
      () 
  in
  let  vbox2 = GPack.vbox ~packing:main#add () in
  let intro =
    GMisc.label
      ~xalign:0.0 
      ~text:(i18n "If you want to have dynamic parts or perform some computations
you can use expressions enclosed between #, for example :

The length of AB is #length(A,B)# and 2*3=#2*3#
#pi+e#
#if area(A,B,C) > area(B,C,D) then \"bigger\" else \"smaller\"#
"
)
      ~packing:(vbox2#pack ~expand:false) ()
  in
  
  let text_buffer = GText.buffer 
      ~text:current_text () in
  let textframe = GBin.frame  
      ~border_width:3 
      ~shadow_type:`ETCHED_IN 
      ~packing:(vbox2#pack ~expand:true) () in
  let text = GText.view
      ~buffer:text_buffer 
      ~editable:true
      ~width:400
      ~height:100 
      ~wrap_mode:`WORD
      ~packing:textframe#add () in
  
  let help =  GBin.frame
    ~border_width:6
    ~label:(i18n "Help")  
    ~packing:(vbox#pack ~expand:false)
      () 
  in
  let help_content =
    GMisc.label
      ~xalign:0.0 
      ~text:(i18n "You can use the following fonctions :

+,-,*,/,^
sin,cos,tan,arcsin,arccos,arctan,
sqrt,ln,exp,log,pow,
abs,min,max,
<,>,<=,>=,=,<>
and,or,not,
signed_area,area,angle,length
collinear,between,left_turn,
parallel,orthogonal,
eq_lengths,eq_angles

The following constants :

true,false,e,pi

The following constructions :

if cond then expr else expr
let id = expr in expr
")
      ~packing:help#add ()
  in
  help#misc#hide();
  let hboxbuttons = GPack.hbox ~packing:(vbox#pack ~expand:false) () in
  let button_help = GButton.toggle_button
      ~label:(i18n "Help")
      ~packing:hboxbuttons#add () in
  ignore (button_help#connect#clicked 
    ~callback:(fun () -> if button_help#active then 
      help#misc#show () 
    else help#misc#hide () ));
  let button_cancel = GButton.button
      ~label:(i18n "Cancel")
      ~packing:hboxbuttons#add () in
  let button_ok = GButton.button
      ~label:(i18n "Ok")
      ~packing:hboxbuttons#add () in
  ignore (button_cancel#connect#clicked  ~callback:w#destroy);
  ignore (button_ok#connect#clicked ~callback:
	    (fun () -> 
	      x#change_style (STYLE_TEXTE {texte=(text_buffer#get_text ())});
	      x#recalcule;
	      redraw_fun();
	      w#destroy()
	    )); 
  w#show ()
  

