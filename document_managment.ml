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

exception CanNotCancel
exception MoreThanOne

type t_construction_list = (t_objet_graphique) list

class type t_studied_fact =
  object
    method output_coq : unit -> Fol.fol Formulas.formula option
    method output_predicate : unit -> Fol.fol Formulas.formula option
    method dependancies : t_objet_graphique list
    method recompute : unit
  end
 


type t_all_object =
    {
      mutable constructions : t_construction_list;
      mutable studied_facts : t_studied_fact list
    }


type page =
    {
      mutable title : string;
      mutable page_description : string;
      
      mutable objects : t_all_object;
   
      undo_stack : t_all_object Stack.t;
      redo_stack : t_all_object Stack.t;
    }
      
type document = 
    {
      mutable filename : string;
      mutable author : string;
      mutable pages : page list;
      mutable pages_to_detect_changes : page list;
      mutable current_page_number : int;
    }
     
let initial_constructions =
  let r = new Objets_repere.objet_repere in
    r#nomme "Grid";
    r#change_style (STYLE_REPERE {grid_shown=true});
    r#cache_objet;
    [r]
      
let initial_objects =
  {
    constructions = initial_constructions;
    studied_facts = []
  }

let initial_page =
  {
    title = "Page 1";
    page_description = (i18n "Put your description here.");
    objects = initial_objects;
    undo_stack = Stack.create ();
    redo_stack = Stack.create ();
  }
    
let initial_document = 
  {
    filename = "document1.drg";
    author = (i18n "anonymous");
    pages = [initial_page];
    pages_to_detect_changes = [initial_page];
    current_page_number = 0;
  }

let documents = ref [initial_document]
let current_document_nb = ref 0

let change_current_document n = 
  current_document_nb := n

let get_document n () = List.nth !documents n
let get_current_page () =  
  let doc = get_document !current_document_nb () in
  List.nth doc.pages doc.current_page_number

let new_page doc_n () = 
  let d = get_document doc_n () in
  let page = 
    {
      title = "Page "^string_of_int ((List.length d.pages)+1);
      page_description = (i18n "Put your description here.");
      objects = initial_objects;
      undo_stack = Stack.create ();
      redo_stack = Stack.create ();
    }
 in
    d.pages <- page::d.pages
    
let new_document () =
  let number =  (List.length !documents)+1 in
  let newdoc =
    {
      filename = "document"^(string_of_int number)^".drg";
      author = "anonymous";
      pages = [initial_page];   
      pages_to_detect_changes = [initial_page];   
      current_page_number = 0;
   }
  in
  documents := !documents@[newdoc];
  number-1
    

let delete_current_page () =
  let p = get_current_page () in   
    Stack.push p.objects p.undo_stack;
    p.objects.constructions <- [];
    p.objects.studied_facts <- []

let rec list_remove n l = match n with
      0 -> List.tl l
    | n -> (List.hd l)::(list_remove (n-1) (List.tl l))

let remove_document doc_n () =
    documents := list_remove doc_n !documents  

let add_object (obj:t_objet_graphique) =
  let p = get_current_page () in   
    Stack.push p.objects p.undo_stack;
    p.objects.constructions <- obj::p.objects.constructions
 
let add_object_facts obj =
   let p = get_current_page () in   
    Stack.push p.objects p.undo_stack;
     p.objects.studied_facts <- obj::p.objects.studied_facts
 

let rec depend o1 o2 =
  o1 = o2 ||
  (List.mem o2 o1#dependance) || 
    List.exists (fun o -> (depend o o2)) o1#dependance 

let depending_objects obj =
 let p = get_current_page () in
   List.filter 
     (fun o -> (depend o obj)
     ) 
     p.objects.constructions
   

  
let remove_object (obj:t_objet_graphique) =
  let p = get_current_page () in
    Stack.push p.objects p.undo_stack;
    p.objects.constructions <- List.filter 
      (fun o -> not (depend o obj)
      )  p.objects.constructions
 
let undo () =
   let p = get_current_page () in   
    try 
      Stack.push p.objects p.redo_stack;
       p.objects <- Stack.pop p.undo_stack
    with
	Stack.Empty -> raise CanNotCancel

let redo () = 
  let p = get_current_page () in   
  try 
    Stack.push p.objects p.undo_stack;
    p.objects <- (Stack.pop p.redo_stack)
  with
      Stack.Empty -> raise CanNotCancel

let has_changed_since_last_saved_or_opened () =
  let d = get_document  !current_document_nb () in
    d.pages = d.pages_to_detect_changes 
      (* TODO deal with author changes *)

let update_after_save_or_open  () =
  let d = get_document !current_document_nb  () in
    d.pages_to_detect_changes <- d.pages

let get_current_constructions () = 
  let p = get_current_page () in
    p.objects.constructions

let get_current_studied_facts () = 
  let p = get_current_page () in
    p.objects.studied_facts
