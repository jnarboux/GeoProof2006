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

(* Name, Description, Figure, Proof, ... *)

let theorems_list = [
"Pythagore","a^2 + b^2 = c^2";
"Ceva","bidule"
]

let make_model data = 
	let cols = new GTree.column_list in
	let name_col = cols#add Gobject.Data.string in
	let descr_col = cols#add Gobject.Data.string in
	let l = GTree.list_store cols in 

	List.iter
	( fun (name,descr) -> 
	let row = l#append () in
	l#set ~row ~column:name_col name;
	l#set ~row ~column:descr_col descr)
	data;

	l, (name_col, descr_col)

let make_view_aux (model,(name_col, descr_col)) packing =
	let renderer =  GTree.cell_renderer_text [] in
	let view_name_col =  GTree.view_column 
		~title:"Names" 
		~renderer:(renderer,["text",name_col]) () in	
	let view_descr_col =
	let col = GTree.view_column ~title:"Descriptions" () in
	let str_renderer = GTree.cell_renderer_text [] in	
	col#pack str_renderer;
	col#add_attribute str_renderer "text" descr_col;
	col in 		
	let v = GTree.view ~model ~packing () in
	v#append_column view_name_col;
	v#append_column view_descr_col;
	v 

let make_view = make_view_aux (make_model theorems_list) 

(*
let main =
	let w = GWindow.window () in
	let v = make_view w#add in
	w#show();
	GMain.main ()

*)
