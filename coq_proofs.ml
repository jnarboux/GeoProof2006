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
open Document_managment
open Options
open Optionsdeconfiguration
open Types_de_base

type proof_state = Not_Started | Definition | Proof
let current_proof_state = ref Not_Started

let defined_points = ref []

let copy_to_clipboard text () =
  let clipboard = (GData.clipboard Gdk.Atom.clipboard) in
    clipboard#set_text text

(* A queue for the messages send to CoqIde *)

let fifo_messages = Queue.create ()

(* A private clipboard *)

let clipboard_geoproof = GData.clipboard (Gdk.Atom.intern "_GeoProof")

(* Put the massage in the queue *)

let  copy_to_coq_ide text () =
  Queue.add text fifo_messages

let send_messages () =
  while true do
    if not (Queue.is_empty fifo_messages) then
      (match clipboard_geoproof#text with 
	   None | Some ("Ack") ->
	     clipboard_geoproof#set_text (Queue.pop fifo_messages) 
	 | Some(_) -> ());
    Thread.delay 0.1
  done

(* Replace spaces by underscores *)

let remove_spaces s () =
  try
    while (true) do
      let space = String.index s (' ') in
	String.set s space ('_')
    done
  with
      _ -> ()



let send_to_coq_geometric_context () =
  (* Requires *)
  let requires = 
    match !!coq_geom_language with 
	Narboux -> 
"Require Export area_method.\nImport F_scope."
      | Guilhot -> 
"Require Export geometrie_plane."
  in
  let section_name = ((Document_managment.get_current_page()).title) in
    remove_spaces section_name ();
    let section = "Section "^section_name^"." in
      copy_to_coq_ide (requires^"\n"^section) ()
	
let define_point name () =
  let p = 
    match !!coq_geom_language with 
	Narboux -> "Point"
      | Guilhot -> "PO"
  in

  if not (List.mem name !defined_points) then
    begin
      defined_points := name::!defined_points;
      copy_to_coq_ide ("Variable "^name^": "^p^".") ()
    end


let output_coq_for_creation o = 
  match !current_proof_state with   
    | Not_Started -> ()
    | Definition -> 
	begin
	  if List.mem o#objet_type [TYPE_DROITE;TYPE_SEGMENT;TYPE_CERCLE;TYPE_DEMI_DROITE] then
	    let (a,b) = 
	      match o#defining_points () with
		Some(a) -> a
	      | None -> assert false
	    in
	    define_point a ();
	    define_point b ();
	  else if o#objet_type = TYPE_POINT then
	    define_point o#objet_nom ();
	  match o#output_coq_body() with
	    None -> ()
	  | Some formula ->
	      let text_pred = 
		string_printer formula ~curryfied:true;
		(Format.flush_str_formatter ())
	      in
	      copy_to_coq_ide ("Hypothesis H"^o#objet_nom^": "^text_pred^".") ()
	end
    | Proof ->  
	begin
	  match o#output_coq_tactic() with
	      None -> ()
	    | Some a ->	copy_to_coq_ide (a^".") ()
	end

let is_started () = 
  !current_proof_state <> Not_Started

let stop_proof () = (* TODO empty coqide *)
  defined_points := [];
  current_proof_state := Not_Started
	
let start_proof () = 
  send_to_coq_geometric_context ();
  current_proof_state := Definition;
  Construction.iter (fun o -> output_coq_for_creation o)	

(* This function translates a GeoProof predicate (based on line and circles)
 into a Coq predicate depending on the laguage used *)

let translate_formula formula = 
  let get_points x () =
    match x with 
	Var s ->  
	  begin
	    let o = Construction.get_object s in
	      match o#defining_points () with
		  None ->  failwith "erreur translate formula"
		| Some (a,b) -> Var(a),Var(b)
	  end
      | _ -> failwith "erreur translate formula"
  in
  let translate_pred_name s =
    match !!coq_geom_language with 
	Narboux ->
	  (match s with
	      "collinear" -> "Col"
	    | "eq_points" -> "="
	    | _ -> s)
      | Guilhot ->
	  (match s with 
	      "collinear" -> "alignes"
	    | "eq_points" -> "="
	    | "parallel" -> "paralleles"
	    | "orthogonal" -> "perpendiculaires"

	    | _ -> s)
  in
  let translate_pred atom = 
    match atom with
      | R("eq_points"|"collinear"|"left_turn"|"between" as s,l) -> 
	  R(translate_pred_name s,l) 
      | R("eq_angles" as s,[a;b;c;d;e;f]) ->
	  begin
	    match 
	      !!coq_geom_language with 
		  Narboux -> R(translate_pred_name s,[a;b;c;d;e;f]) 
		| Guilhot -> 
		    let ab = Fn("droite",[a;b]) in
		    let bc = Fn("droite",[b;c]) in
		    let de = Fn("droite",[d;e]) in
		    let ef = Fn("droite",[e;f]) in
		    R("=",[Fn("cons_AD",[ab;bc]);Fn("cons_AD",[de;ef])])
	  end
      | R("eq_length" as s,[s1;s2]) ->
	  let a,b = get_points s1 () in
	  let c,d = get_points s2 () in
	    begin
	      match 
		!!coq_geom_language with 
		    Narboux ->
		      R(translate_pred_name s,[a;b;c;d])
		  | Guilhot ->
		      R("=",[Fn("distance",[a;b]);Fn("distance",[c;d])])
	    end
      | R("parallel"|"orthogonal" as s,[s1;s2]) ->
	  let a,b = get_points s1 () in
	  let c,d = get_points s2 () in
	    begin
	      match 
		 !!coq_geom_language with 
		     Narboux ->
		       R(translate_pred_name s,[a;b;c;d])
		   | Guilhot ->
		       R(translate_pred_name s,[Fn("droite",[a;b]);Fn("droite",[c;d])])
	    end
      | _ -> failwith "predicate unknown"
  in
  let translate x = Atom(translate_pred x) in
    Formulas.onatoms translate formula


let really_output_coq_for_goal ?asser:(assertion=false) pred () =
  let formula =  translate_formula pred in
  let statement =   
    string_printer ~curryfied:true formula;
    (Format.flush_str_formatter ()) 
  in
    if assertion then
      copy_to_coq_ide ("assert ("^statement^").") ()
    else
      begin
	copy_to_coq_ide ("Goal "^statement^".") ();
	copy_to_coq_ide "Proof." ();
	current_proof_state := Proof
      end

let output_coq_for_goal pred () =
  match !current_proof_state with   
    | Not_Started -> start_proof (); really_output_coq_for_goal pred ()
    | Definition ->  really_output_coq_for_goal pred ()
    | Proof -> really_output_coq_for_goal ~asser:true pred ()



let output_coq_for_delete o () = 
  match o#objet_type with
      TYPE_POINT -> copy_to_coq_ide  ("clear "^o#objet_nom^".") ()
    | _ ->  ()





(* This function build the statement from a conclusion *)
(* If there is no conclusion it computes the conjonction of all studied facts *)

let statement conclusion = 
  let conclusion = 
    match conclusion with 
	None -> Construction.predicates ()
      | Some a -> a
  in
  let default_hypothesis_formula = 
    (* First we select the hypothesis *)
    let list_predicates = 
      Construction.map (fun x -> if x#objet_type <> TYPE_TEXTE then x#output_coq_body () else None) in
      (* We remove Some() *)
    let list_predicates_clean = 
      List.flatten (List.map 
		      (fun x -> 
			 match x with 
			     Some(a) -> [a] 
			   | None -> []) 
		      list_predicates) 
    in
      
      Prop.simplify 
	(List.fold_left (fun x -> fun y -> And(x,y)) True list_predicates_clean) 
  in
    
  let default_hypothesis =
    string_printer ~curryfied:true default_hypothesis_formula;
    (Format.flush_str_formatter ())
  in

  let point_list = String.concat " " ((fv default_hypothesis_formula)@(fv conclusion)) in


  let default_conclusion = 
    string_printer conclusion;
    (Format.flush_str_formatter ()) in
    
  let statment = 
    "Goal forall " ^
      point_list ^
      ", " ^
      default_hypothesis ^
      " -> " ^
      default_conclusion ^
      "." 
  in 
    statment
  
let copy_to_clipboard_statement conc () =
  copy_to_clipboard (statement conc) ()

let copy_to_coq_ide_statement conc () =
  copy_to_coq_ide (statement conc) ()



	
