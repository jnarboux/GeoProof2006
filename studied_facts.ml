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

let add_fact gui name descr color () = 
  let row = gui.model#append () in
  gui.model#set ~row ~column:gui.cols.name name;
  gui.model#set ~row ~column:gui.cols.descr descr;
  gui.model#set ~row ~column:gui.cols.color color;
  row

let modify_fact gui row name descr color () =
  gui.model#set ~row ~column:gui.cols.name name;
  gui.model#set ~row ~column:gui.cols.descr descr;
  gui.model#set ~row ~column:gui.cols.color color
    
let make_model = 
  let cols = new GTree.column_list in
  let name_col = cols#add Gobject.Data.string in
  let descr_col = cols#add Gobject.Data.string in
  let color_col = cols#add Gobject.Data.string in
  let l = GTree.list_store cols in 
    l, (name_col, descr_col, color_col)
      
let make_view_aux (model,(name_col, descr_col, color_col)) packing =
  let renderer =  GTree.cell_renderer_text [] in
  let view_name_col =  GTree.view_column 
    ~title:"Names" 
    ~renderer:(renderer,["text",name_col]) () in	
  let view_descr_col =
    let col = GTree.view_column ~title:"Descriptions" () in
    let str_renderer = GTree.cell_renderer_text [] in	
    col#pack str_renderer;
    col#add_attribute str_renderer "text" descr_col;
    col#add_attribute str_renderer "foreground" color_col;  
    col 
  in 		
  let v = GTree.view ~model ~packing () in
 (*   v#append_column view_name_col; *)
    v#append_column view_descr_col;
    v, 
  {
    model = model;
    cols = 
      {
	name = name_col;
	descr = descr_col;
	color = color_col
      }
  }
      
let make_view = make_view_aux make_model 

