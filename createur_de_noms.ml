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
open Optionsdeconfiguration
open Construction

let numero_texte = ref 0
let numero_repere = ref 0
let numero_point = ref 0
let numero_droite = ref 0
let numero_cercle = ref 0
let numero_vecteur = ref 0
let numero_segment = ref 0
let numero_mark = ref 0

let rec construit_nom s n = 
  incr n;
  let name = String.concat ("_") ([s ; string_of_int !n]) in
  if exist_object name then construit_nom s n else name

let nom_nouveau_texte () = construit_nom !!prefixe_nom_textes numero_texte
let nom_nouveau_repere () = construit_nom !!prefixe_nom_reperes numero_repere

let rec nom_nouveau_point () = 
  if (!numero_point < 26) then
    begin
      incr numero_point;
      let n = int_of_char 'A' in
      let name = String.make 1 (char_of_int  (!numero_point + n -1)) in
      if exist_object name then nom_nouveau_point () else name
    end
  else
    construit_nom !!prefixe_nom_points numero_point
      
let nom_nouveau_droite () = construit_nom !!prefixe_nom_droites numero_droite
let nom_nouveau_cercle () = construit_nom !!prefixe_nom_cercles numero_cercle
let nom_nouveau_vecteur () = construit_nom !!prefixe_nom_vecteurs numero_vecteur
let nom_nouveau_segment () = construit_nom !!prefixe_nom_segments numero_segment
let name_new_mark () = construit_nom !!prefixe_nom_mark numero_mark

let reset_name_generation () =
 numero_texte := 0;
 numero_repere := 0;
 numero_point := 0;
 numero_droite := 0;
 numero_cercle := 0;
 numero_vecteur := 0;
 numero_segment := 0;
 numero_mark := 0
