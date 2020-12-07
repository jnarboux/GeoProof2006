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
 
open I18n
open Objets_graphiques
open Fol
open Formulas

exception User_interrupt

type automatic_method = Chou | Wu | Grobner

let false_version formula = 
  match formula with
    Imp(h,g) -> Imp(h,False)
  | _ -> assert false

let start_proof meth formula = 
  try 
    let result = 
      match meth with
      | Wu -> Geom.wubis formula
      | Chou -> Debug.pdb "Chou"; false
      | Grobner -> Grobner.grobner_decide formula
    in
    if result then if 
      Grobner.grobner_decide (false_version formula) then
      (i18n "<span foreground=\"darkgreen\">The theorem is true</span><span foreground=\"red\"> (because the hypotheses are contradictory!)</span>" )
    else
      (i18n "<span foreground=\"darkgreen\">The theorem is true</span>" )
    else (i18n 
	    "<span foreground=\"red\">The theorem is false</span>")
  with
  | User_interrupt -> (i18n 
	    "<span foreground=\"red\">The computation was stopped !</span>")
  | _ ->  (i18n 
	    "<span foreground=\"red\">Sorry, this can not be solved !</span>")

(* Choose a signal depending on the platform *)
let vt_signal =
  match Sys.os_type with
  | "Win32" -> Sys.sigterm
  |_ -> Sys.sigvtalrm

let interrupt = ref None

let force_interrupt old_action_ref n =
  (* This function is called just before the thread's timeslice ends *)
  if Some(Thread.id(Thread.self())) = !interrupt then
    raise User_interrupt;
  match !old_action_ref with
  | Sys.Signal_handle f -> f n
  | _ -> failwith "Not in threaded mode"
      
let create_window conclusion () =
  let conclusion = True in
(*
    match conclusion with 
	None -> Construction.predicates ()
      | Some a -> a
  in
  *)  
  let old_action_ref = ref Sys.Signal_ignore in
  let old_action = 
    Sys.signal vt_signal (Sys.Signal_handle (force_interrupt old_action_ref)) in
  old_action_ref := old_action;
  let w = GWindow.window ~title:(i18n "Automatic theorem proving") 
    ~allow_shrink:false ~allow_grow:true () in
  let vbox = GPack.vbox ~packing:w#add () in
  let hbox  = GPack.hbox ~packing:vbox#add () in
  let vbox2 =  GPack.vbox ~packing:hbox#add () in
  let help =  GBin.frame
    ~border_width:6
    ~label:(i18n "Help")  
    ~packing:hbox#pack
    () in
  let help_content =
    GMisc.label
      ~xalign:0.0 
      ~text:(i18n "You can use the following predicates :
- is_midpoint
- collinear
- parallel
- perpendicular
- is_intersection
- =
- lengths_eq
- angles_eq")
      ~packing:help#add ()
  in
  help#misc#hide ();

  let first = GBin.frame
    ~border_width:6
    ~label:(i18n "-1- Choose the fact you want to check :")  
    ~packing:(vbox2#pack ~expand:true)
    () in
  
  let vboxfirst = GPack.vbox ~packing:first#add () in
  let hyp = GMisc.label
    ~xalign:0.1
    ~text:(i18n "Hypothesis :")  
    ~packing:(vboxfirst#pack ~expand:false)
    () in  
(*  
  let current_figure = GButton.radio_button 
      ~label:(i18n "Current figure")
      ~packing:vboxfirst#add () in 
  let edit_manually = GButton.radio_button 
      ~group:current_figure#group
      ~label:(i18n "Edit manually")
      ~packing:vboxfirst#add () in 
 *)

  let default_hypothesis = 
    (* First we select the hypothesis *)
    let list_predicates = 
      Construction.map 
	(fun x -> if 
	  x#objet_type <> TYPE_TEXTE && x#objet_type <> TYPE_MARK  
	then x#predicate_form () else None) in
      (* We remove Some() *)
    let list_predicates_clean = 
      List.flatten (List.map 
		      (fun x -> 
			match x with 
			  Some(a) -> [a] 
			| None -> []) 
		      list_predicates) 
    in
    
    let formula = Prop.simplify 
	(List.fold_left (fun x -> fun y -> And(x,y)) True list_predicates_clean) in
    string_printer formula;
    (Format.flush_str_formatter ())
  in
  
  let hypothesis_buffer = GText.buffer 
      ~text:default_hypothesis () in
  let hypothesisframe = GBin.frame  
      ~border_width:3 
      ~shadow_type:`ETCHED_IN 
      ~packing:vboxfirst#add () in
  let hypothesis = GText.view
      ~buffer:hypothesis_buffer 
      ~editable:true
      ~width:400
      ~height:100 
      ~wrap_mode:`WORD
      ~packing:hypothesisframe#add () in
(*
   goal#misc#hide (); (* set_sensitive false; *)
   edit_manually#connect#clicked ~callback:(fun () ->    goal#misc#show ());
   current_figure#connect#clicked ~callback:(fun () -> (* TODO change text to figure state *)   goal#misc#hide () (* set_sensitive false *));
*)
  
  let conc = GMisc.label
      ~xalign:0.1
      ~text:(i18n "Conclusion :")  
      ~packing:(vboxfirst#pack ~expand:false)
      () in

  let default_conclusion =    
    string_printer ~curryfied:true conclusion;
    (Format.flush_str_formatter ()) in
  
  let conclusion_buffer = GText.buffer 
      ~text:default_conclusion () in
  let conclusionframe = GBin.frame  
      ~border_width:3 
      ~shadow_type:`ETCHED_IN 
      ~packing:vboxfirst#add () in
  let conclusion = GText.view
      ~buffer:conclusion_buffer 
      ~editable:true
      ~width:400
      ~height:100
      ~wrap_mode:`WORD
      ~packing:conclusionframe#add () in

  let second = GBin.frame 
      ~label:(i18n "-2- Choose the method you want to use :")  
      ~border_width:6
      ~packing:(vbox2#pack ~expand:false)
      () in
  let vbox_choices =  GPack.vbox ~packing:second#add () in
  let choice = ref Grobner in
  let grobner = GButton.radio_button 
      ~label:(i18n "Gröbner bases method")
      ~packing:vbox_choices#add () in 

  let wu = GButton.radio_button 
      ~group:grobner#group
      ~label:(i18n "Wu method")
      ~packing:vbox_choices#add () in
 
  let chou = GButton.radio_button 
      ~group:grobner#group    
      ~label:(i18n "Chou method")
      ~packing:vbox_choices#add () in
  chou#misc#set_sensitive false;
  
  let result = GBin.frame
      ~border_width:6
      ~label:(i18n "-3- Get the result")
      ~packing:(vbox2#pack ~expand:false) ()
  in
  
  let vboxresult = GPack.vbox ~packing:result#add () in

  let hboxresult = GPack.hbox ~packing:vboxresult#add () in
 
  let button_ok = GButton.button
      ~label:(i18n "Start searching for a proof")
      ~packing:hboxresult#add () in

  let button_interrupt = GButton.button
      ~label:(i18n "Interrupt")
      ~packing:hboxresult#add () in
  button_interrupt#misc#hide ();
  let current_computation = ref None in
  ignore (button_interrupt#connect#clicked 
    ~callback:(fun () -> 
      match !current_computation with
	None -> ()
      | Some a -> 
	  Debug.pdb "Try interrupting the computation";
	  interrupt:=Some(Thread.id a)
	      ));
  
  let last_formula = 
    let text =  (hypothesis#buffer#get_text ())^" ==> "^(conclusion#buffer#get_text ()) in
    let lexbuf = Lexing.from_string text in
    let formula = Parser_logic.start Lexer_logic.token lexbuf in 
    let formula = 
      try 
	Geom.originate formula 
      with _ -> formula
    in
      ref formula
  in
  let last_answer = ref None in
  let last_method = ref Grobner in

  let result_text = GMisc.label
      ~text:(i18n "The theorem is ???")  
      ~packing:vboxresult#add
      () in
  
  ignore (button_ok#connect#clicked ~callback:
    (fun () ->
      let formula = !last_formula in
      button_ok#misc#set_sensitive false;	 
      button_ok#set_label (i18n "Work in progress, please wait");
      button_interrupt#misc#show ();
      let prove form =
	let text = start_proof !choice form in
	last_answer := Some text;
	last_method := !choice;
	result_text#set_use_markup true;
	result_text#set_label text;
	result_text#misc#show ();
	button_ok#set_label (i18n "Finished");
	button_interrupt#misc#hide ()
      in
      current_computation := Some (Thread.create prove formula);
    ));
    
  let hboxbuttons = GPack.hbox ~packing:(vbox#pack ~expand:false) () in
  let button_help = GButton.toggle_button
      ~label:(i18n "Help")
      ~packing:hboxbuttons#add () in
  ignore (button_help#connect#clicked 
    ~callback:(fun () -> if button_help#active then 
      help#misc#show () 
    else help#misc#hide () ));
  let button_close = GButton.button
      ~label:(i18n "Close")
      ~packing:hboxbuttons#add () in
  ignore (button_close#connect#clicked  ~callback:w#destroy);
    
  let when_changed () =
    try 
      let newtext =  (hypothesis#buffer#get_text ())^" ==> "^(conclusion#buffer#get_text ()) in
      let newlexbuf = Lexing.from_string newtext in
      let newformula = Parser_logic.start Lexer_logic.token newlexbuf in 
      let newformula = Geom.originate newformula in 
	(* The new buffer is syntatically correct *)
	(* We compare with the old abstract tree *)
	if !last_formula = newformula & !last_method = !choice  then
	  match !last_answer with
	    | Some a -> 	  
		result_text#set_label  a;
		button_ok#misc#set_sensitive false;      
		button_ok#set_label (i18n "Finished");
	    | None ->
		begin
		  button_ok#misc#set_sensitive true;
		  button_ok#set_label (i18n "Start searching for a proof");
		  result_text#set_label (i18n "The theorem is ???")	
		end
	else
	  begin 
	    last_formula := newformula;
	    button_ok#misc#set_sensitive true;
	    button_ok#set_label (i18n "Start searching for a proof");
	    result_text#set_label (i18n "The theorem is ???")
	  end
    with
	_ ->
	  button_ok#misc#set_sensitive false;
	  button_ok#set_label (i18n "The statement contains an error");
	  result_text#set_label (i18n "The theorem is ???")
  in
  ignore (hypothesis_buffer#connect#changed ~callback:when_changed);
  ignore (conclusion_buffer#connect#changed ~callback:when_changed);  
  ignore (grobner#connect#clicked ~callback:(fun () -> choice := Grobner;when_changed())); 
  ignore (wu#connect#clicked ~callback:(fun () -> choice := Wu;when_changed()));
  ignore (chou#connect#clicked ~callback:(fun () -> choice := Chou;when_changed()));  
  w#show ()

