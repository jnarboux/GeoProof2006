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

open Types_de_base
open Repere
open Couche
open Fonctions
open Options
open Optionsdeconfiguration
open Geometric_functions.GeometryFunctions
open Printf
open Fol
open Formulas

exception Not_computable

type t_type_objet =
  | TYPE_POINT
  | TYPE_DROITE
  | TYPE_VECTEUR
  | TYPE_SEGMENT
  | TYPE_DEMI_DROITE
  | TYPE_CERCLE
  | TYPE_REPERE
  | TYPE_MARK
  | TYPE_TEXTE

(* all expect repere *)

let tout_type_objet = [TYPE_POINT; 
		       TYPE_DROITE; 
		       TYPE_VECTEUR;
		       TYPE_CERCLE;
		       TYPE_TEXTE;
		       TYPE_SEGMENT;
		       TYPE_DEMI_DROITE;
		       TYPE_MARK
		      ]

type style_objet =
  | BLOP
  | STYLE_POINT of style_point
  | STYLE_DROITE of style_droite
  | STYLE_REPERE of style_repere
  | STYLE_TEXTE of style_texte
  | STYLE_MARK of style_mark


type t_donnees_objet =
  | DIST of float 
  | COORDS_POINT of t_coords
  | COORDS_DEUX_POINTS of t_coords * t_coords 
  | EQ_DROITE of t_line_equation
  | EQ_CERCLE of t_circle_equation

type highlight_type =
  | MOVABLE
  | CONSTRUCTION
  | NEAR


class type virtual t_objet_graphique = 
  object
    method objet_nom : string
    method nomme : string -> unit
    method objet_style : style_objet
    method objet_couleur : GDraw.color
    method change_couleur : GDraw.color -> unit
    method objet_selectionne : bool
    method selectionne_objet : unit
    method deselectionne_objet : unit
    method objet_couche : int
    method change_couche : int -> unit
    method objet_visible : bool
    method change_visibilite : bool -> unit
    method montre_objet : unit
    method cache_objet : unit
    method highlighted : bool
    method highlight_object : highlight_type -> unit
    method unhighlight_object : highlight_type -> unit   

    method lock_object : unit
    method unlock_object : unit
    method locked : bool

    method trace_object : unit
    method untrace_object : unit
    method traced : bool

    method objet_calculable : bool
    method rend_calculable : unit
    method rend_incalculable : unit

    method objet_epaisseur : int
    method change_epaisseur : int -> unit

    method change_style : style_objet -> unit
    method parametres : t_donnees_objet
    method change_parametres : t_donnees_objet -> unit
    method utiliser_style_et_couleur : GDraw.pixmap -> unit
    method fixer_style_et_couleur_pour_mise_en_valeur : GDraw.pixmap -> unit
    method dessiner :  GDraw.pixmap -> GDraw.pixmap -> Repere.t -> unit
    method depend_de : t_objet_graphique -> bool
    method bouge : t_coords -> unit
    method private couleur_a_utiliser : unit -> GDraw.color

	(* Output natural language *)
    method private def_langue_naturelle_attributs : unit -> string
    method def_langue_naturelle : unit -> string
    method virtual def_langue_naturelle_corps : unit -> string

	(* Output SVG *)
    method private output_svg_attributs : unit -> string*string
    method output_svg : unit -> Xml.xml
    method virtual output_svg_body : unit -> Xml.xml

	(* Output Kig *)
    method private output_kig_attributs : unit -> Xml.xml
    method output_kig : unit -> Xml.xml
    method virtual output_kig_body : unit -> Xml.xml list

	(* Output Car *)
    method private output_car_attributs : unit -> (string*string) list
    method output_car : unit -> Xml.xml
    method virtual output_car_body : unit -> Xml.xml

	(* Output DrG *)
    method output_drg_attributs : unit -> Xml.xml
    (* method output_drg : unit -> Xml.xml *)
    method virtual output_drg_body : unit -> Xml.xml

	(* Output Coq *)
    method output_coq : unit -> Xml.xml
    method virtual output_coq_body : unit -> Fol.fol Formulas.formula option
    method output_coq_tactic : unit -> string option
   
	(* Output Eukleides *)
    method output_eukleides : unit -> string
    method virtual output_eukleides_body : unit -> string

	(* Output pst-eucl *)
    method output_pst_eucl : unit -> string
    method virtual output_pst_eucl_body : unit -> string


	(* Output Datas *)
    method coordonnees_point : t_coords
    method coordonnees_premier_point : t_coords
    method coordonnees_second_point : t_coords
    method equation_droite : t_line_equation
    method equation_cercle : t_circle_equation
      
    method virtual objet_type : t_type_objet
    method virtual dessine :  GDraw.pixmap -> Repere.t -> unit
    method virtual est_proche : Repere.t -> t_coords -> bool
    method virtual recalcule : unit
    method virtual dependance : t_objet_graphique list
    method virtual movable : unit -> bool   
    method virtual defining_points : unit -> (string * string) option
    method virtual predicate_form : unit -> Fol.fol Formulas.formula option   
  end


class virtual objet_graphique : t_objet_graphique =
  object (self)
    val mutable nom = ""
    method objet_nom = nom
    method nomme n = nom <- n

    val mutable style = BLOP
    method objet_style = style
    method change_style sty = style <- sty

    val mutable selectionne = false
    method objet_selectionne = selectionne
    method selectionne_objet = selectionne <- true
    method deselectionne_objet = selectionne <- false
	
    val mutable couleur = (`BLACK: GDraw.color)
    method objet_couleur = couleur 
    method change_couleur c = couleur <- c
	
    val mutable couche = 1
    method objet_couche = couche
    method change_couche i = couche <- i
 
    val mutable epaisseur = 1
    method objet_epaisseur = epaisseur
    method change_epaisseur e = epaisseur <- e
	
    method private couleur_a_utiliser () = 
      match couches.(self#objet_couche-1).couleur with 
	None -> self#objet_couleur
      | Some c -> c

    method utiliser_style_et_couleur (win:GDraw.pixmap) = 
      let style_a_utiliser style = 
	match style with
	  (STYLE_DROITE PLEINE) -> `SOLID
	| (STYLE_DROITE POINTILLEE) -> `ON_OFF_DASH
	| _ -> `SOLID 
      in
      win#set_foreground (self#couleur_a_utiliser());
      win#set_line_attributes ~style:(style_a_utiliser style) ~width:epaisseur  ~cap: `ROUND ()
	
    method fixer_style_et_couleur_pour_mise_en_valeur (win:GDraw.pixmap) =
      win#set_foreground (Couleurs.light_color (self#couleur_a_utiliser()));
      win#set_line_attributes 
	~style:`SOLID 
	~width:(epaisseur+3)  
	~cap: `ROUND ()

    method dessiner (win:GDraw.pixmap) (back:GDraw.pixmap) rep =
      if not couches.(self#objet_couche-1).cache then
	begin	
	  if self#objet_selectionne || self#highlighted then
	    begin
	      self#fixer_style_et_couleur_pour_mise_en_valeur win;
	      self#dessine win rep;
	    end;
	  if self#traced then
	    begin
	      self#utiliser_style_et_couleur back;
	      self#dessine back rep;
	    end;
	  self#utiliser_style_et_couleur win;
	  self#dessine win rep
        end

    (* Par défaut on suppose qu'un objet ne peut pas être bougé *)
    method bouge (_ : t_coords) = ()
	  
    val mutable visible = true
    method objet_visible = visible
    method change_visibilite b = visible <- b
    method montre_objet = visible <- true
    method cache_objet = visible <- false

    val mutable highlighted = []
    method highlighted = (highlighted <> [])
    method highlight_object s = highlighted  <- s::highlighted
    method unhighlight_object s = 
      let rec remove t l = match l with
	  [] -> []
	| a::r -> if t=a then (remove t r) else a::(remove t r)
      in
      highlighted <- remove s highlighted

    val mutable locked = false
    method locked = locked
    method lock_object = locked  <- true
    method unlock_object = locked <- false
      
    val mutable traced = false
    method traced = traced
    method trace_object = traced  <- true
    method untrace_object = traced <- false
	
    val mutable calculable = true
    method objet_calculable = calculable
    method rend_calculable = calculable <- true
    method rend_incalculable = calculable <- false
	
    val mutable donnees = None

    method parametres : t_donnees_objet = 
      match donnees with
	None -> raise Not_computable
      | (Some d) -> d

    method change_parametres datas = donnees <- Some(datas)

    method depend_de o = List.memq o self#dependance

    method coordonnees_point =
      match self#parametres with
      | COORDS_POINT c -> c
      | _ -> assert false
	    
    method coordonnees_premier_point =
      match self#parametres with
      | (COORDS_DEUX_POINTS(_,c2)) -> c2
      | _ -> assert false
	    
    method coordonnees_second_point =
      match self#parametres with
      | (COORDS_DEUX_POINTS(c1,_)) -> c1
      | _ -> assert false

    method equation_droite =
      match self#parametres with
      | EQ_DROITE eq -> eq
      | COORDS_DEUX_POINTS(c1,c2) -> 
	  Geometric_functions.GeometryFunctions.line_from_two_points c1 c2
      | _ -> assert false

    method equation_cercle =
      match self#parametres with
      | EQ_CERCLE eq -> eq
      | _ -> assert false

    method def_langue_naturelle () =
      let att = (self#def_langue_naturelle_attributs ()) in
      let corps = (self#def_langue_naturelle_corps ()) in
      "Let "^corps^att^"."
	
    method private def_langue_naturelle_attributs () =
      let couleur = 
	match self#objet_type with
	| TYPE_POINT ->
	    begin
		if self#objet_couleur = !!couleur_point_defaut then [] else
	      let c = Couleurs.string_of_color self#objet_couleur in
	      ["in "^c]
	    end
	| _ ->
	     begin
	      match Couleurs.string_of_color self#objet_couleur with
		"black" -> []
	      | c -> ["in "^c]
	     end
      in
      let style =
	if self#objet_style = STYLE_POINT !!style_point_defaut then []
	else
	match self#objet_style with
	| STYLE_POINT ROND -> ["depicted as a circle"]
	| STYLE_POINT CROIX -> ["depicted as a cross"]
	| STYLE_POINT RONDPLEIN -> ["depicted as filled circle"]
	| STYLE_POINT CARRE -> ["depicted as a box"]
	| STYLE_POINT CARREPLEIN -> ["depicted as a filled box"]
	| STYLE_DROITE POINTILLEE -> ["dashed"]
	| _ -> []
      in
      let epaisseur =
	match self#objet_type with
	| TYPE_POINT | TYPE_DROITE | TYPE_VECTEUR | TYPE_SEGMENT | TYPE_CERCLE -> 
	    begin
	      match self#objet_epaisseur with
		1 -> ["thin"]
	      | 2 -> []
	      | 3 -> ["thick"]
	      | 4 -> ["very thick"]
	      | _ -> []
	    end
	| _ -> []
      in
      let visibilite =
	if self#objet_visible then [] else 
	begin
	  match self#objet_type with
	     | TYPE_DROITE -> ["hiden"]
	     | _ -> ["hiden"]
	end
      in
      
      let rec ajoute_virgule l = 
	match l with
	  [] -> ""
	| [a] -> a
	| a::r -> a^", "^(ajoute_virgule r)
      in
      let tous = ajoute_virgule (couleur@style@epaisseur@visibilite) in
      if tous = "" then "" else " ("^tous^")"



    method output_svg () =  
      match self#output_svg_body () with
	Xml.Element(name,att,child) -> 
	  Xml.Element(name,(self#output_svg_attributs ())::att,child)
      | _ -> assert false
	    
    method private output_svg_attributs () =
      let color = Couleurs.rgb_of_color self#objet_couleur in
      let (red,green,blue) = color in
      let color = "rgb("^
	(string_of_int red)^","^
	(string_of_int green)^","^
	(string_of_int blue)^");" 
      in
      let width = 
	"stroke-width:"^(string_of_int self#objet_epaisseur)^";"
      in
      let style = 
	match self#objet_style with
	  STYLE_DROITE POINTILLEE -> "stroke-dasharray:3;fill:none;"
	| STYLE_DROITE PLEINE -> "fill:none;"      
	| STYLE_POINT (CARREPLEIN|RONDPLEIN) -> "fill:"^color^";"
	| STYLE_POINT _ -> "fill:none;"
	| STYLE_REPERE _ ->  "fill:none;"
	| STYLE_TEXTE _ -> "fill:"^color^";stroke-width:0;font-size:12;font-style:normal;font-weight:100;font-family:Sans;"
	| STYLE_MARK _ -> "fill:none;"
	| BLOP -> "fill:none;"
     (* The style of a point impact the way it is drawn and the svg style *)
      in
      "style","stroke:"^color^width ^style

      (* Car output *)

    method output_car () = Xml.Element ("",[],[])
    method private output_car_attributs () = [("","")]

      (* Kig output *)

    method output_kig () = Xml.Element ("",[],[])
    method private output_kig_attributs () = Xml.Element ("",[],[])

      (* Drg output *)

(*
    method output_drg () =  
      match self#output_drg_body () with
	  Xml.Element(name,att,child) -> 
	    Xml.Element(name,att,child)
	| _ -> assert false
*)

    method output_drg_attributs () =
      let name = self#objet_nom in
      let color = Couleurs.rgb_of_color self#objet_couleur in
      let (red,green,blue) = color in
      let red = string_of_int red in
      let green = string_of_int green in
      let blue = string_of_int blue in 
      let width = string_of_int self#objet_epaisseur in
      let style = 
	match self#objet_style with
	    STYLE_DROITE POINTILLEE -> "line-style","DashLine"
	  | STYLE_DROITE PLEINE -> "line-style","SolidLine"      
	  | STYLE_POINT CARREPLEIN -> "point-style","Rectangular"
	  | STYLE_POINT RONDPLEIN -> "point-style","Round"
	  | STYLE_POINT CARRE -> "point-style","RectangularEmpty"
	  | STYLE_POINT ROND -> "point-style","RoundEmpty"
	  | STYLE_POINT CROIX -> "point-style","Cross"
	  | STYLE_REPERE {grid_shown=b} ->  "grid-style",(string_of_bool b)
	  | STYLE_TEXTE {texte=s} -> ("text",s)
	  (* | STYLE_MARK _ -> "toto","" *)
	  | BLOP -> "blop",""
	  | _ -> "toto",""	  (* TODO *)    
      in
      Xml.Element ("Attributs",[
		   ("name",name);
		   ("width",width);
		   ("shown",if self#objet_visible then "true" else "false");
		   style
		     (* TODO style line point *)
		 ],[Xml.Element("Color",[("red",red);("green",green);("blue",blue)],[])])
	

	(* Coq output *)

    method output_coq () = Xml.Element ("",[],[])
	
	(* Eukleides output *)
	
    method output_eukleides () =
      let color = 
	Couleurs.eukleides_color_of_color self#objet_couleur in
      let thickness = string_of_int self#objet_epaisseur in (* TODO thickness *)
      let style = 
	match self#objet_style with
	    STYLE_DROITE POINTILLEE -> ",dashed"
	  | STYLE_DROITE PLEINE -> ",full"      
	  | STYLE_POINT CARREPLEIN -> ",box"
	  | STYLE_POINT RONDPLEIN -> ",dot"
	  | STYLE_POINT CROIX -> ",cross"
	  | STYLE_POINT ROND -> ",dot"
	  | STYLE_POINT CARRE -> ",box"
	  | STYLE_REPERE _ ->  ""
	  | STYLE_TEXTE _ -> ""
	  | STYLE_MARK _ -> ""
	  | BLOP -> ""
      in
      let drawing_command =
	(* We make a special case for labels, 
	   because labels do not exist in Eukleides language *)	
	if self#objet_visible && self#objet_type = TYPE_VECTEUR then (* hack: to draw a vector we need the strarting point *)
	  sprintf "\ncolor(%s)\nthickness(%s)\ndraw(%s,%s%s)" color thickness self#objet_nom (List.hd (self#dependance))#objet_nom style
	else if self#objet_visible && self#objet_type <> TYPE_TEXTE then
	  sprintf "\ncolor(%s)\nthickness(%s)\ndraw(%s%s)" color thickness self#objet_nom style
	else ""
      in
	(self#output_eukleides_body ())^drawing_command

	(* pst-eucl output *)

    method output_pst_eucl () = ""			

      (* By default it is none *)
    method output_coq_tactic () = None
       
    method virtual output_kig_body : unit -> Xml.xml list
    method virtual output_coq_body : unit -> Fol.fol Formulas.formula option 
    method virtual output_car_body : unit -> Xml.xml
    method virtual output_svg_body : unit -> Xml.xml
    method virtual output_drg_body : unit -> Xml.xml
    method virtual output_eukleides_body : unit -> string
    method virtual output_pst_eucl_body : unit -> string
    method virtual def_langue_naturelle_corps : unit-> string

    method virtual objet_type : t_type_objet
    method virtual dessine :  GDraw.pixmap -> Repere.t -> unit
    method virtual est_proche : Repere.t -> t_coords -> bool
    method virtual recalcule : unit
    method virtual dependance : t_objet_graphique list
    method virtual defining_points : unit -> (string * string) option
    method virtual predicate_form : unit -> Fol.fol Formulas.formula option
    method virtual movable : unit -> bool

end


let unpack o = match o with None -> assert false | Some a -> a
