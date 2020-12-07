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

open Options
open Optionsdeconfiguration
open I18n  
open Objets_graphiques
open Types_de_base

type document_gui =
    {
      studied_facts : studied_facts_gui;
      pane : GPack.paned;
      notebook : GPack.notebook;
      drawing : GDraw.drawable;
      drawingbuffer : GDraw.pixmap;
      drawingbackbuffer : GDraw.pixmap;
      svg_area : GMisc.image;
      svg_event_box : GBin.event_box;
      text_area : GText.view;
      area : GMisc.drawing_area;
    }
      
let zero_point = [None;None;None]
let one_point = [Some (TYPE_POINT);None;None]
let two_points = [Some (TYPE_POINT);Some(TYPE_POINT);None]
let three_points = [Some (TYPE_POINT);Some(TYPE_POINT);Some(TYPE_POINT)]

let preds =
  [
   "are collinear",(three_points,zero_point);
   "is parallel to",(two_points,two_points);
   "is perpendicular to",(two_points,two_points);
   "is the midpoint of",(one_point,two_points);
   "is equal to",(one_point,one_point);
   "is on the left of",(one_point,one_point);
   "is between",(one_point,one_point);
   "is of the same length as",(two_points,two_points)
 ]


let create_document_tab (notebookdocs:GPack.notebook) nb () =
  let area = GMisc.drawing_area () in
  let pane = GPack.paned `HORIZONTAL ~border_width:5 () in 
  let mylabel= GMisc.label ~text:("Document"^(string_of_int (nb+1))) () in 
  notebookdocs#append_page ~tab_label:mylabel#coerce  pane#coerce;
 (*  notebookdocs#goto_page nb; *)

  let frame_notebook = GBin.frame ~shadow_type:`IN ~packing:(pane#pack1 ~shrink:true)  () in
  let notebook = GPack.notebook ~packing:frame_notebook#add () in
  
  let labelsvg = GMisc.label ~text:"SVG Preview" () in
  let labeltext= GMisc.label ~text:"Natural language" () in
  let labelpix = GMisc.label ~text:"Graphic window" () in
  
  let text_area = GText.view () in
  
  let svg_event_box = GBin.event_box () in
  let svg_area = GMisc.image ~packing:svg_event_box#add () in
  
  notebook#insert_page ~tab_label:labelpix#coerce  ~pos:1 area#coerce; 
  notebook#insert_page ~tab_label:labeltext#coerce ~pos:2 text_area#coerce;
  notebook#insert_page ~tab_label:labelsvg#coerce  ~pos:3 svg_event_box#coerce;
  
  ignore (text_area#buffer#create_tag ~name:"not_editable" [`EDITABLE false;`BACKGROUND "green"]);
  area#misc#realize ();
  let pixmap = GDraw.pixmap 2500 2500 () in
  
  (* The back_pixmap is used to keep a trace of some objects, it is not cleared at each step *) 
  let back_pixmap =  GDraw.pixmap 2500 2500 () in
  
  back_pixmap#set_foreground !!couleur_fond;
  back_pixmap#rectangle 
    ~x:0 ~y:0
    ~width:2500 ~height:2500 
    ~filled:true ();
   
  (*********************************)
  (* Interactive Theorem proving : *)
  (*********************************)  
    
  (* Facts list *)

  let frame_facts = GBin.frame ~shadow_type:`IN ~packing:pane#pack2 () in
    
  let hpane = GPack.paned `VERTICAL ~packing:frame_facts#add () in

  let hpane_theorems =  GPack.paned `VERTICAL ~packing:hpane#pack1 () in
  let hpane_goals =     GPack.paned `VERTICAL ~packing:hpane#pack2 () in

  (* Theorems *)

  let frame_theorems =  GBin.frame ~shadow_type:`IN ~packing:hpane_theorems#pack1 () in
  let expander_theorems = GBin.expander 
    ~label:(i18n "Theorems")  
    ~packing:frame_theorems#add () in
 

(*
  let theorems =  GTree.view ~packing:expander_theorems#add () in 
*)

  let theorems_view = Theorems.make_view expander_theorems#add in

  (* Studied facts *)
    
  let frame_studied_facts =  GBin.frame ~shadow_type:`IN ~packing:hpane_theorems#pack2 () in
  let expander_studied_facts = GBin.expander 
    ~label:(i18n "Studied properties")  
    ~packing:frame_studied_facts#add () in


  let vboxsf = GPack.vbox ~packing:expander_studied_facts#add () in
  let hboxsf = GPack.hbox ~packing:(vboxsf#pack ~expand:false) () in
  
  let button_add_sf = GButton.button
      ~label:(i18n "Add")
      ~packing:(hboxsf#pack ~expand:false) () in

  let create_combo l () = GEdit.combo_box ~packing:(hboxsf#pack ~expand:false) 
      (*~popdown_strings:l *) ~width:40 () in
  
  let args_left = Array.init 3 (fun i -> create_combo ["A";"B"] ()) in

  let combo_preds = GEdit.combo ~packing:hboxsf#add 
      ~popdown_strings:[(i18n "are collinear");
			(i18n "is parallel to");
			(i18n "is perpendicular to");
			(i18n "is the midpoint of");
			(i18n "is equal to");
			(i18n "is on the left of");
			(i18n "is between");
			(i18n "is of the same length as");
		      ]
      () in

  let args_right = Array.init 2 (fun i -> create_combo ["A";"B"] ()) in

  let studied_facts_view,studied_facts_list = 
    Studied_facts.make_view vboxsf#add in 
  
    (* Selection of a row *)
    studied_facts_view#selection#connect#after#changed ~callback:
    begin fun () ->
      prerr_endline "selection changed";
      List.iter
        (fun p -> Debug.pdb (GtkTree.TreePath.to_string p))
	studied_facts_view#selection#get_selected_rows ;
      
    end;
    
    (* Double click *)
    studied_facts_view#connect#after#row_activated 
      ~callback:(fun path vcol -> Debug.pdb "activated");
      
  (* Known facts *)
    
  let frame_known_facts =  GBin.frame ~shadow_type:`IN ~packing:hpane_goals#pack1 () in
  let expander_known_facts = GBin.expander   
    ~label:(i18n "Hypothesis")
    ~packing:frame_known_facts#add () in

  let known_facts =  GTree.view ~packing:expander_known_facts#add () in 
    
  (* Goals *)
  
  let frame_goals =  GBin.frame ~shadow_type:`IN ~packing:hpane_goals#pack2 () in
  let expander_goals = GBin.expander   
    ~label:(i18n "Goals")
    ~packing:frame_goals#add () in

  let goals = GTree.view ~packing:expander_goals#add () in 

  expander_studied_facts#set_expanded true;
  expander_known_facts#set_expanded true;
  expander_goals#set_expanded true;
  expander_theorems#set_expanded true;
  frame_facts#misc#hide ();   

  let drawing = new GDraw.drawable (area#misc#window) in
    {
      studied_facts = studied_facts_list;
      pane = pane;
      notebook = notebook;
      drawing = drawing;
      drawingbuffer = pixmap;
      drawingbackbuffer = back_pixmap; 
      svg_area = svg_area;
      svg_event_box = svg_event_box;
      text_area = text_area;
      area = area;
    }

    

