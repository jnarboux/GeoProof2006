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


open Geometric_functions.GeometryFunctions
open Types_de_base
open Objets_graphiques
open Objets_repere
open Document_managment
  
exception AnnulationImpossible
exception MoreThanOne

let iter f =
  List.iter f (List.rev (get_current_constructions ()))
  
let rec constructions_before obj l = 
  match l with
      [] -> []
    | a::r -> 
	if a#objet_nom = obj 
	then r 
	else constructions_before obj r
  
let iter_before obj f =
  List.iter f (constructions_before obj (get_current_constructions ()))

let map f = 
  List.map f (List.rev (get_current_constructions ()))

let exists p = 
  List.exists p (get_current_constructions ())
    
let filter p =
  List.filter p (get_current_constructions ())

let bouger_objet obj c =
  obj#bouge c;
  List.iter 
    (fun x -> if depend x obj then x#recalcule)
    (List.rev (get_current_constructions ()));

  List.iter 
    (fun x -> if List.mem obj x#dependancies then x#recompute)
    (get_current_studied_facts ()) 
    (* 
	 This assume that object are in the reverse order 
	 of their construction 
      *)


let predicates () =
  let texts = 
    List.filter 
      (fun o -> o#objet_type = TYPE_TEXTE ||  o#objet_type = TYPE_MARK)
      (get_current_constructions ())
  in
  let preds = 
    List.map 
      (fun o -> o#predicate_form())
      texts
  in
  let preds = List.filter (fun p -> p<>None) preds in
  let unpack s = match s with None -> assert false | Some a -> a in
  let preds = List.map unpack preds in
  let put_and x y = Formulas.And(x,y) in
  Prop.simplify (List.fold_left put_and Formulas.True preds)
    
let objets_proches rep c tl exceptions =
  let condition x = not (List.mem x exceptions) in
  List.filter 
    (function x -> 
       x#objet_visible 
       && (x#est_proche rep c) 
       && (List.mem x#objet_type tl)
       && (condition x))
    (get_current_constructions ())
    
let objets_libres_proches rep c tl exceptions = 
  let condition x = not (List.mem x exceptions) in
  List.filter 
    (function x -> 
       x#objet_visible 
       && (x#est_proche rep c) 
       && (x#movable ())
       && (not x#locked) 
       && (List.mem x#objet_type tl)
       && (condition x))
    (get_current_constructions ())
    
let highlight_movable_objects () =
  List.iter 
    (fun x -> if (x#movable ()) then x#highlight_object MOVABLE) 
    (get_current_constructions ())
    
let unhighlight_objects s () =
  List.iter 
    (fun x -> x#unhighlight_object s) 
    (get_current_constructions ())
    
    
let exist_object name = 
  List.exists 
    (fun o -> o#objet_nom = name) 
    (get_current_constructions ())
  
let get_object nom =
  match 
    (List.filter 
       (fun o -> o#objet_nom = nom) 
       (get_current_constructions ())) 
  with
      [] -> 
	Printf.eprintf "L'objet %s n'a pas été trouvé\n" nom;
	raise Not_found
    | [x] -> x
    | _ -> raise MoreThanOne
	
let zoom_ajuste () =
  let minx = ref (of_float infinity) in
  let miny = ref (of_float infinity) in
  let maxx = ref (of_float (-. infinity)) in
  let maxy = ref (of_float (-. infinity)) in
  let update x min max =
    if (x <! !min) then min := x;
    if (x >! !max) then max := x
  in
  let ajuste_cadre_a_objet o =
    match o#objet_type with
      | TYPE_POINT -> 
	  update o#coordonnees_point.x minx maxx;
	  update o#coordonnees_point.y miny maxy
      | TYPE_CERCLE -> 
	let e = o#equation_cercle in
	  update (e.center.x -! e.radius)  minx maxx;
	  update (e.center.x +! e.radius)  minx maxx;
	  update (e.center.y -! e.radius)  miny maxy;
	  update (e.center.y +! e.radius)  miny maxy
      | TYPE_TEXTE ->
 	  update o#coordonnees_point.x minx maxx;
	  update o#coordonnees_point.y miny maxy
	    
	   (* TODO take into account texte size *)
	    
      | _ -> assert false (* these objets have already been filtered out *)
  in

  let liste_objets_visibles = 
    List.filter 
      (fun o -> o#objet_visible && (List.mem o#objet_type [TYPE_POINT;TYPE_CERCLE;TYPE_CERCLE;TYPE_TEXTE])) (get_current_constructions ()) in
    if liste_objets_visibles = [] 
    then ()  
    else
      begin
	List.iter ajuste_cadre_a_objet liste_objets_visibles;
	let agrandx = (!maxx -! !minx) *! (of_float 0.1)  in
	let agrandy = (!maxy -! !miny) *! (of_float 0.1)  in  
	  minx := !minx -! agrandx;
	  maxx := !maxx +! agrandx;
	  miny := !miny -! agrandy;
	  maxy := !maxy +! agrandy; 
	  
	  Repere.zoomAjuste !minx !miny !maxx !maxy;
      end
