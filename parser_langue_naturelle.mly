%{

(*******************************************************************************)
(* GeoProof, an interactive geometry tool writen in OCaml.                    *)
(* Copyright (C) 2005 Julien Narboux et Arnaud Doniec                          *)
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

open Couleurs
open Analyse_semantique_langue_naturelle
open Types_de_base
open Options
open Optionsdeconfiguration

let def_att = {couleur=(!!couleur_defaut);pointille=false;visi=true;epaisseur=(!!epaisseur_defaut);numero_couche=1;style_point=(!!style_point_defaut)}
let def_att_p = {def_att with couleur=(!!couleur_point_defaut)}

%}

%token SOIT
%token SLASH
%token UN UNE LA LE
%token <int> INTEGER
%token VIRGULE
%token NORME
%token CROCHET_OUV CROCHET_FER 
%token PARENT_OUV PARENT_FER
%token DE EN
%token POINT DROITE CERCLE SEGMENT VECTEUR REPERE
%token QUELCONQUE
%token LONGUEUR
%token INTERSECTION
%token MILIEU MEDIATRICE BISSECTRICE
%token PERPENDICULAIRE PARALLELE
%token ET A AVEC GRILLE
//les geo-couleurs...
%token ROUGE VERT BLEU NOIR ORANGE JAUNE ROSE VIOLET GRIS BLANC
%token COUCHE EPAIS TRES_EPAIS FIN POINTILLE CACHE
%token FORME CARRE ROND PLEIN
%token CENTRE PASSANT PAR SUR SOUS
%token <string> IDENT
%token EOL EOF

%start start
%type <unit> start
%%

start:
|  commande_etoile EOF {$1}
;

commande_etoile:
| /* epsilon */ {()}
| commande commande_etoile {$1;$2}
;  

commande:
| declaration {$1}
;

declaration:
  SOIT decl_point EOL {($2 def_att_p) }
| SOIT decl_point attributs EOL { ($2 $3) }
| SOIT decl_line EOL { ($2 def_att) }
| SOIT decl_line attributs EOL { ($2 $3)}
| SOIT decl_line2 EOL { ($2 def_att) }
| SOIT decl_line2 attributs EOL { ($2 $3) }
| SOIT decl_circle EOL { ($2 def_att) }
| SOIT decl_circle attributs EOL { ($2 $3) }
/*
| SOIT UN REPERE AVEC GRILLE {(repere )}
| SOIT UN REPERE {()}
*/
;

decl_end:  
| EOL {}
|  {print_string "error decl_end" }
;

attributs:
| PARENT_OUV list_of_attributs PARENT_FER { $2 }
;

forme:
| CARRE {CARRE}
| ROND {ROND}
| ROND PLEIN {RONDPLEIN}
| CARRE PLEIN {CARREPLEIN}

list_of_attributs:
| EN couleur {{def_att with couleur=$2}}
| EN POINTILLE {{def_att with pointille=true}}
| CACHE {{def_att with visi=false}}
| FIN  {{def_att with epaisseur=1}}
| EPAIS  {{def_att with epaisseur=3}}
| TRES_EPAIS  {{def_att with epaisseur=4}}
| SUR LA COUCHE INTEGER  {{def_att with numero_couche=$4}}
| SOUS FORME DE forme {{def_att with style_point=$4}}

| EN couleur VIRGULE list_of_attributs {{$4 with couleur=$2}}
| EN POINTILLE VIRGULE list_of_attributs {{$4 with pointille=true}}
| CACHE VIRGULE list_of_attributs {{$3 with visi=false}}
| FIN VIRGULE list_of_attributs {{$3 with epaisseur=1}}
| EPAIS VIRGULE list_of_attributs {{$3 with epaisseur=3}}
| TRES_EPAIS VIRGULE list_of_attributs {{$3 with epaisseur=4}}
| SUR LA COUCHE INTEGER VIRGULE list_of_attributs {{$6 with epaisseur=$4}}
| SOUS FORME DE forme VIRGULE list_of_attributs {{$6 with style_point=$4}}
;

couleur:
| ROUGE {red}
| VERT {green}
| BLEU {blue}
| ORANGE {orange}
| JAUNE {yellow}
| ROSE {pink}
| NOIR {black}
| BLANC {white}
| GRIS {grey}
| VIOLET {purple}
;

decl_point:
  IDENT UN POINT quelconque { point $1}
| IDENT UN POINT DE IDENT { point_on $1 $5}
| IDENT LE MILIEU DE IDENT { middle2 $1 $5}
| IDENT LE MILIEU DE IDENT ET IDENT { middle $1 $5 $7}
| point_intersection { $1 }
| list_of_point INTEGER POINT quelconque
	{ if (List.length $1)==$2 
		then multiple_point $1 
		else raise (Wrong_number_declaration ($2,"points")) 
	}
;

quelconque:
/* epsilon */ {}
| QUELCONQUE {}
;

point_intersection:
  IDENT INTERSECTION DE id_line ET id_line 
    { intersection $1 $4 $6}
| IDENT INTERSECTION DE IDENT ET IDENT 
    { intersection $1 $4 $6}
| IDENT INTERSECTION DE IDENT ET id_line 
    { intersection $1 $4 $6}
| IDENT INTERSECTION DE id_line ET IDENT 
    { intersection $1 $4 $6}
| IDENT ET IDENT INTERSECTION DE IDENT ET id_line 
    { double_intersection $1 $3 $6 $8}
| IDENT ET IDENT INTERSECTION DE id_line ET IDENT	      
    { double_intersection $1 $3 $6 $8}
| IDENT ET IDENT INTERSECTION DE IDENT ET IDENT	      
    { double_intersection $1 $3 $6 $8}

;

list_of_point:
  IDENT { [point $1] }
| IDENT VIRGULE list_of_point { (point $1)::$3 }
;

decl_line:
| IDENT UNE DROITE QUELCONQUE 
	{ any_line $1}
| IDENT LA DROITE PASSANT PAR IDENT ET IDENT 
	{ line $1 $6 $8}
| IDENT LE SEGMENT PASSANT PAR IDENT ET IDENT 
	{ segment $1 $6 $8}
| IDENT LE VECTEUR PASSANT PAR IDENT ET IDENT 
	{ vecteur $1 $6 $8}
| id_line LA DROITE PARALLELE A id_line PASSANT PAR IDENT 
	{ parallel $1 $6 $9}
| IDENT LA DROITE PARALLELE A id_line PASSANT PAR IDENT 
	{ parallel $1 $6 $9}

/*
| id_line LE SEGMENT PARALLELE A id_line PASSANT PAR IDENT ET DE LONGUEUR NORME IDENT IDENT NORME 
	{ parallel_seg $1 $6 $9 $14 $15}
| IDENT LE SEGMENT PARALLELE A id_line PASSANT PAR IDENT ET DE LONGUEUR NORME IDENT IDENT NORME 
	{ parallel_seg $1 $6 $9 $14 $15}
*/

| id_line LA DROITE PERPENDICULAIRE A id_line PASSANT PAR IDENT 
	{ perpendicular $1 $6 $9}
| IDENT LA DROITE PERPENDICULAIRE A id_line PASSANT PAR IDENT 
	{ perpendicular $1 $6 $9}
/*
| id_line LE SEGMENT PERPENDICULAIRE A id_line PASSANT PAR IDENT ET DE LONGUEUR NORME IDENT IDENT NORME 
	{ perpendicular_seg $1 $6 $9 $14 $15}
| IDENT LE SEGMENT PERPENDICULAIRE A id_line PASSANT PAR IDENT ET DE LONGUEUR NORME IDENT IDENT NORME 
	{ perpendicular_seg $1 $6 $9 $14 $15}
*/

| IDENT LA MEDIATRICE DE IDENT ET IDENT 
	{ perpendicular_bissector $1 $5 $7}
| id_line LA MEDIATRICE DE IDENT ET IDENT 
	{ perpendicular_bissector $1 $5 $7}
| IDENT LA BISSECTRICE DE PARENT_OUV IDENT VIRGULE IDENT VIRGULE IDENT PARENT_FER
	{ bissecting $1 $6 $8 $10}
;

decl_line2:
| LA DROITE PARENT_OUV IDENT VIRGULE IDENT PARENT_FER 
	{ line ("l"^$4^$6) $4 $6}
| LA DROITE PARENT_OUV IDENT IDENT PARENT_FER 
	{ line ("l"^$4^$5) $4 $5}
| LE SEGMENT CROCHET_OUV IDENT VIRGULE IDENT CROCHET_FER 
	{ segment ("s"^$4^$6) $4 $6}
| LE SEGMENT CROCHET_OUV IDENT  IDENT CROCHET_FER 
	{ segment ("s"^$4^$5) $4 $5}
| LE VECTEUR CROCHET_OUV IDENT VIRGULE IDENT CROCHET_FER 
	{ vecteur ("v"^$4^$6) $4 $6}
| LE VECTEUR CROCHET_OUV IDENT  IDENT CROCHET_FER 
	{ vecteur ("v"^$4^$5) $4 $5}
;

id_line:
| PARENT_OUV IDENT VIRGULE IDENT PARENT_FER 
	{ ("l"^$2^$4) }
| PARENT_OUV IDENT  IDENT PARENT_FER 
	{ ("l"^$2^$3) }
| CROCHET_OUV IDENT VIRGULE IDENT CROCHET_FER 
	{ ("s"^$2^$4) }
| CROCHET_OUV IDENT IDENT CROCHET_FER 
	{ ("s"^$2^$3) }
| CROCHET_OUV IDENT IDENT PARENT_FER 
	{ ("s"^$2^$3) }
;


decl_circle:
| IDENT LE CERCLE DE CENTRE IDENT PASSANT PAR IDENT 
    {circle $1 $6 $9}






