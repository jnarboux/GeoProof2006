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

open Geometric_functions.GeometryFunctions
open Options
open Optionsdeconfiguration
open Types_de_base
open Objets_graphiques
open Objets_texte
open Fonctions
open Repere
open Printf

class type t_objet_texte_clique =
object
  inherit t_objet_texte
  method recalcule : unit
  method depend_de : t_objet_graphique -> bool
  method dependance : t_objet_graphique list
  method bouge : t_coords -> unit 
  method def_langue_naturelle_corps : unit -> string
  
  method output_car_body : unit -> Xml.xml
  method output_kig_body : unit -> Xml.xml list
  method output_drg_body : unit -> Xml.xml
  method output_eukleides_body : unit -> string
  method output_pst_eucl_body : unit -> string
  method predicate_form : unit -> Fol.fol Formulas.formula option
end


class objet_texte_clique coordsinit textinit =
object (self)
  inherit objet_texte as obp
    
  method def_langue_naturelle_corps () =
    "be the label \""^(self#get_text ())^"\""

  method recalcule = 
    let new_text = 
      Evaluation_expressions.eval_string (self#get_text ()) 	 
    in
      self#set_text_value new_text;
      
  method depend_de _ = false
  method dependance =
    Evaluation_expressions.dependancies_string (self#get_text ())  
      (* TODO calculer une seule fois + parsing 1 fois *)

  method bouge c =
    self#change_parametres (COORDS_POINT c);
    
  method output_car_body () =
    Xml.Element("",[],[])
      
  method output_kig_body () =
    [Xml.Element("",[],[])]
      
  method output_drg_body () =
    let to_string x = to_string x !!precision in 
      Xml.Element("Text",
		  [
		    ("name",self#objet_nom);   
		    ("x",to_string self#coordonnees_point.x);
		    ("y",to_string self#coordonnees_point.y);
		  ],[])
	
  (* TODO Remove slashs in latex strings *)
	
  method output_eukleides_body () =
    let to_string x = to_string x !!precision in 
    let x = to_string self#coordonnees_point.x in
    let y =  to_string self#coordonnees_point.y in
      sprintf "draw(\"%s\",point(%s,%s),0:)" (self#get_text ()) x y
 
  method output_pst_eucl_body () = ""
  
  method output_coq_body () = None 

  method predicate_form () =
    Some(Evaluation_expressions.collect_predicates (self#get_text ()))

  initializer
    self#set_text textinit;
    self#bouge coordsinit;
    self#recalcule
end
