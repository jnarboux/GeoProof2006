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

(* $Id: objets_repere.ml,v 1.27 2005/11/16 17:19:35 jnarboux Exp $ *)

open Geometric_functions.GeometryFunctions
open Types_de_base
open Fonctions
open Repere
open Options
open Optionsdeconfiguration
open Objets_graphiques
open List

class type t_objet_repere =
  object
    inherit t_objet_graphique
    method def_langue_naturelle_corps : unit -> string
    method recalcule : unit
    method depend_de : t_objet_graphique -> bool
    method dependance : t_objet_graphique list
    method objet_type :  t_type_objet 
    method dessine :  GDraw.pixmap -> Repere.t -> unit
    method est_proche : Repere.t -> t_coords -> bool
    method movable : unit -> bool

    method output_svg_body : unit -> Xml.xml
    method output_car_body : unit -> Xml.xml
    method output_kig_body : unit -> Xml.xml list
    method output_drg_body : unit -> Xml.xml
    method output_coq_body : unit -> Fol.fol Formulas.formula option
    method output_eukleides_body : unit -> string
    method output_pst_eucl_body : unit -> string
    method predicate_form : unit -> Fol.fol Formulas.formula option
    method defining_points : unit -> (string * string) option
  end
 
class objet_repere =
  object (self)
    inherit objet_graphique

    val precision = 2 (* Number of digits of the grid labels *)

    method private grid_lines_list () =  
      let rep = Repere.get() in
      let i n = papier_vers_ecran_t rep {x = n; y = of_int 0} in
      let j n = papier_vers_ecran_t rep {x = of_int 0; y = n} in
      let l = ref [] in
      
      
      let deltax = x_max() -! x_min() in
      let deltay = y_max() -! y_min() in
      let delta = min deltax deltay in
      let step = delta /!  (of_int 10) in

      let linex k = 
	(((i k).x,(of_int 0)),
	 ((i k).x,!screen_width),
	 ((i k).x +! two,(i k).y -! (of_int 3)),
	 (to_string k precision)) 
      in
      let liney k = 
	((of_int 0,(j k).y),
	 (!screen_width,(j k).y),
	 ((j k).x +! (of_int 3),(j k).y -! two),
	 (to_string k precision)) 
      in

      let k = ref step in
      while (!k <! (x_max()))
      do
	l:= (linex !k)::!l;
	k := !k +! step
      done;
      let k = ref (neg step) in
      while (!k >! (x_min()))
      do
	l := (linex !k)::!l;
	k := !k -! step
      done;
      
      let k = ref step in
      while (!k <! (y_max()))
      do
	l := (liney !k)::!l;
	k := !k +! step
      done;
      let k = ref (neg step) in
      while (!k >! (y_min()))
      do
	l := (liney !k)::!l;
	k := !k -! step
      done;
      !l


    method dessine win rep =
      let origine =  origine_ecran_int rep in 
      let screen_width_int = to_int !screen_width in
      let screen_height_int = to_int !screen_height in

      (* axis drawing *)
	win#line 0 origine.yint screen_width_int origine.yint;
	win#line origine.xint 0 origine.xint screen_height_int;
      
      (* grid drawing *)
      match self#objet_style with
	  STYLE_REPERE style -> if style.grid_shown then
	    begin
	      win#set_foreground (Couleurs.grey);
	      win#set_line_attributes ~style:`ON_OFF_DASH ();
	      List.iter 
		(fun (p,q,r,s) ->	
		   Font.draw win s (to_int (fst r)) (to_int (snd r));
		   win#line (to_int (fst p)) (to_int (snd p)) (to_int (fst q)) (to_int (snd q))
		) 
		
		(self#grid_lines_list ())	      
	    end
	| _ -> () (* assert false TODO *)


    method est_proche rep c =
      let origine = origine_ecran_t rep in
      let proche = (of_int  !!pixels_proche)  /! two in
      abs (c.x -! origine.x) <! proche ||
      abs (c.y -! origine.y) <! proche	

    method objet_type = TYPE_REPERE

    method dependance = []

    method recalcule = ()

    method def_langue_naturelle_corps () = " some axes"^
      match self#objet_style with
	STYLE_REPERE style ->  if style.grid_shown then
	  " with a grid"
	else ""
      | _ -> "" (* assert false TODO  *)   
	    
    method  output_svg_body () =
      let origine = origine_ecran_t (Repere.get()) in 
      let grid =
	match self#objet_style with
	  STYLE_REPERE style -> 
	    if style.grid_shown then
	      List.map 
		(fun ((x1,y1),(x2,y2),(rx,ry),s) -> 
		  Xml.Element("g",[],[
			      Xml.Element("line",
					  [
					   ("x1",(to_string x1 precision));
					   ("x2",(to_string x2 precision));
					   ("y1",(to_string y1 precision));
					   ("y2",(to_string y2 precision))
					 ],[])
				;
				Xml.Element("text",
					    [
					      ("font-size","12");
					      ("font-style","normal");
					      ("font-weight","100");
					      ("font-family","Sans");
					      ("x",(to_string rx precision));
					      ("y",(to_string ry precision));
					    ],[Xml.PCData s])
				
				   ]))
		(self#grid_lines_list ()) 
	    else []
	| _ ->[] (* assert false TODO *)
	      
      in
      Xml.Element("g",[],[
		  Xml.Element("line",
			      [
			       ("x1",(string_of_float 0.));
			       ("x2",(to_string !screen_width 5));
			       ("y1",(to_string origine.y 5));
			       ("y2",(to_string origine.y 5))
			     ],[]);
		  Xml.Element("line",
			      [("x1",(to_string origine.x 5));
			       ("x2",(to_string origine.x 5));
			       ("y1",(string_of_float 0.));
			       ("y2",(to_string !screen_height 5))
			     ],[]);
		    Xml.Element("g",[("stroke","grey");("stroke-dasharray","5cm,2cm")],grid)
		]

		 )

    method output_car_body () =
      Xml.Element("",[],[])

    method output_kig_body () =
      [Xml.Element("",[],[])]
    
    method output_coq_body () = None

    method output_drg_body () =
      let grid_shown =
	match self#objet_style with
	    STYLE_REPERE style ->  (string_of_bool style.grid_shown) 
	  | _ -> assert false
      in
	
	Xml.Element("Axes",[
		      ("name",self#objet_nom);
                      ("grid_shown",grid_shown)
		    ],[])
	  

    method output_eukleides_body () = ""
    method output_pst_eucl_body () = ""

    method movable () = false
    method predicate_form () = None
    method defining_points () = None

    initializer self#recalcule
  end
