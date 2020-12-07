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

open Options
open Couleurs
open Types_de_base

let bin_dir = Filename.dirname Sys.argv.(0)    
let home_dir = (try Sys.getenv "HOME" with _ -> ".")
let config_dir_basename =
  if  Autoconf.windows 
  then Autoconf.name 
  else "."^Autoconf.name

let config_dir = Filename.concat home_dir config_dir_basename
let config_name = Filename.concat config_dir "config.ini"

let config_ini = create_options_file config_name

let config_section = file_section config_ini [] ""

let couleur_option : GDraw.color option_class = 
  sum_option
  [
   ("blanc", white);
   ("noir",  black);
   ("rouge", red);
   ("vert",  green);
   ("jaune", yellow);
   ("bleu",  blue);
   ("orange",orange);
   ("violet",purple);
   ("gris",  grey);
   ("rose",  pink)
 ]
    

let couleur_fond = define_option config_section ["background_color"] 
    "La couleur du fond (blanc,rouge,vert,jaune,bleu,orange,violet,gris,noir ou rose)." 
    couleur_option white

let couleur_defaut = define_option config_section ["object_color"] 
    "La couleur des objets par défaut (blanc,rouge,vert,jaune,bleu,orange,violet,gris,noir ou rose)."     couleur_option black

let couleur_point_defaut =  define_option config_section ["point_color"] 
    "La couleur par défaut des points (blanc,rouge,vert,jaune,bleu,orange,violet,gris,noir ou rose)." couleur_option red

let epaisseur_defaut =  define_option config_section ["line_width"] 
    "L'épaisseur des objets par défaut (entre 1 et 4)." int_option 2

let style_droite_option = sum_option [("Pleine",PLEINE);("Pointillée",POINTILLEE)]
let style_point_option = sum_option [("Croix",CROIX);("Rond",ROND);("Carre",CARRE);("RondPlein",RONDPLEIN);("CarrePlein",CARREPLEIN)]

let style_droite_defaut = define_option config_section ["line_style"] 
    "Le style par défaut des droites (Pleine ou Pointillée)." style_droite_option PLEINE

let style_point_defaut = define_option config_section ["point_style"] 
    "Le style par défaut des points (Croix,Rond,Carre,CarrePlein ou RondPlein)." style_point_option ROND

let couche_defaut = define_option config_section ["object_layer"] 
    "La couche des objets par défaut (entre 1 et 10)." int_option 1

let prefixe_nom_points = define_option config_section ["points_name_prefix"] 
    "Le prefixe utilisé pour le nom des points" string_option "Point"

let prefixe_nom_droites = define_option config_section ["lines_name_prefix"] 
    "Le prefixe utilisé pour le nom des droites" string_option "Line"

let prefixe_nom_vecteurs = define_option config_section ["vectors_name_prefix"] 
    "Le prefixe utilisé pour le nom des vecteurs" string_option "Vector"
 
let prefixe_nom_cercles =  define_option config_section ["circles_name_prefix"] 
    "Le prefixe utilisé pour le nom des cercles" string_option "Circle"

let prefixe_nom_textes =  define_option config_section ["texts_name_prefix"] 
    "Le prefixe utilisé pour le nom des textes" string_option "Label"

let prefixe_nom_segments =  define_option config_section ["segments_name_prefix"] 
    "Le prefixe utilisé pour le nom des segments" string_option "Segment"

let prefixe_nom_reperes =  define_option config_section ["Grid_name_prefix"] 
    "Le prefixe utilisé pour le nom des repères" string_option "Grid"

let prefixe_nom_mark =  define_option config_section ["Mark_name_prefix"] 
    "Le prefixe utilisé pour le nom des marques" string_option "Mark"


let unité_distance_option = sum_option [("Cms",CMS);("Pouces",POUCES);("Points",POINTS)]
let unité_angle_option = sum_option [("Degres",DEGRES);("Radians",RADIANS)]

let unité_distance = define_option config_section ["distance_unit"] 
    "L'unité de distances (Cms,Pouces ou Points)" unité_distance_option CMS

let unité_angle = define_option config_section ["angle_unit"] 
    "L'unité d'angle (Degres ou Radians)" unité_angle_option DEGRES

let visibilité_barre_outils_principale =  define_option config_section ["main_toolbar_visibility"] 
    "Visibilité de la barre d'outils principale (true ou false)" bool_option true

let visibilité_barre_outils_vue =  define_option config_section ["view_toolbar_visibility"] 
    "Visibilité de la barre d'outils vue (true ou false)" bool_option true

let visibilité_barre_outils_point =  define_option config_section ["point_toolbar_visibility"] 
    "Visibilité de la barre d'outils point (true ou false)" bool_option true

let visibilité_barre_outils_droite =  define_option config_section ["line_toolbar_visibility"] 
    "Visibilité de la barre d'outils droite (true ou false)" bool_option true

let visibilité_barre_outils_cercle =  define_option config_section ["circle_toolbar_visibility"] 
    "Visibilité de la barre d'outils cercle (true ou false)" bool_option true

let visibilité_barre_outils_transformation =  define_option config_section ["transformation_toolbar_visibility"] 
    "Visibilité de la barre d'outils transformation (true ou false)" bool_option true

let visibilité_barre_statut =  define_option config_section ["statusbar_visibility"] 
    "Visibilité de la barre de statut (true ou false)" bool_option true

let  visibilité_barre_outils_facts =  define_option config_section ["facts_toolbar_visibility"] 
    "Visibilité de la barre de tests (true ou false)" bool_option true

let  visibilité_barre_outils_measures =  define_option config_section ["measures_toolbar_visibility"] 
    "Visibilité de la barre de mesures (true ou false)" bool_option true

let  visibilité_barre_outils_labels =  define_option config_section ["labels_toolbar_visibility"] 
    "Visibilité de la barre de labels (true ou false)" bool_option true

let pixels_proche =  define_option config_section ["selection_precision"] 
    "The precision needed for selections (pixels)." int_option 5

let nombre_de_couches =  define_option config_section ["number_of_layers"] 
    "The number of layers that are available." int_option 10

let automatic_point_label = define_option config_section ["automatic_point_labeling"] 
    "Automatically add a label for points while they are created." bool_option true

let number_of_recent_files =  define_option config_section ["number_of_recent_files"] 
    "Number of recent files."  int_option 4

let recent_files_list =  define_option config_section ["recent_files_list"] 
    "Recent files list."  (list_option filename_option) []

let last_folder = define_option config_section ["last_folder"] 
    "The last folder in use."  path_option []

let icons_size =  define_option config_section ["icons_size"] 
    "The size of the icons."  int_option 18

let precision = define_option config_section ["precision"] 
    "The precision of the computations (number of digits)."  int_option 30

let formal_language_option = sum_option [("Narboux",Narboux);("Guilhot",Guilhot)]

let coq_geom_language =  define_option config_section ["formal_language_for_Coq_export"] 
    "The language to be used for communication with Coq."  formal_language_option  Narboux
