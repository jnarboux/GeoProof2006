(*******************************************************************************)
(* GeoProof, an interactive geometry tool writen in OCaml.                    *)
(* Copyright (C) 2004 Julien Narboux                                           *)
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

open Printf
open Lexing
open Lexer_langue_naturelle
open Parser_langue_naturelle
open Analyse_semantique_langue_naturelle


let report_loc (b,e) file =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc


let entree_langue_naturelle file =
  let c = 
    try open_in file
    with 
      e -> eprintf "Anomaly: %s\n@." (Printexc.to_string e);assert false
  in
  let lb = Lexing.from_channel c in
  try
    Parser_langue_naturelle.start Lexer_langue_naturelle.token lb;
    Construction.zoom_ajuste ();
    close_in c;
  with
  | Lexical_error s ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb) file;
      eprintf "lexical error: %s\n@." s
  | Parsing.Parse_error ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb) file;
      eprintf "syntax error\n@."
  | Object_does_not_exist s -> 
      eprintf "%s n'est pas declaré\n@." s
  | Intersection_with_point s -> 
      eprintf "%s est un point,la construction nécessite une droite\n@." s
  | Double_intersection_required s -> 
      eprintf "%s est un cercle,l'intersection donne deux points\n@." s
  | Simple_intersection_required (s1,s2) -> 
      eprintf "%s et %s sont deux droites,l'intersection donne un point\n@." s1 s2
  | X_found_Y_expected(s1,s2,s3) -> 
      eprintf "%s est %s,la construction nécessite %s\n@." s1 s2 s3
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e)
