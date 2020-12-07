{

(*******************************************************************************)
(* GeoProof, an interactive geometry tool writen in OCaml.                    *)
(* Copyright (C) 2005 Julien Narboux and Arnaud Doniec                         *)
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

 
open Lexing
open Parser_langue_naturelle
   
exception Lexical_error of string
exception Eof
exception Shutdown

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum;
      pos_cnum=0 }     
} 
  
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

let ident = alpha ('_'|alpha|digit)* (''')*
let keyword = alpha*

    rule token = parse
| '\n'
    { newline lexbuf; token lexbuf }
| [' ' '\t' '\r']+
    { token lexbuf }
| "/*"
    { comment lexbuf; token lexbuf }
| "//" [^'\n']* ('\n' | eof)
    { newline lexbuf; token lexbuf }
| "soit" {SOIT}
| "Soit" {SOIT}
| "soient" {SOIT}
| "Soient" {SOIT}
| "un" {UN}
| "une" {UNE}
| "en" {EN}
| "la" {LA}
| "le" {LE}
| "de" {DE}
| "avec" {AVEC}
| "sous" {SOUS}
| "forme" {FORME}
| "carré" {CARRE}
| "rond" {ROND}
| "plein" {PLEIN}
| "couche" {COUCHE}
| "épais" {EPAIS}
| "très épais" {TRES_EPAIS}
| "fin" {FIN}
| "grille" {GRILLE}
| "caché" {CACHE}
| "cachée" {CACHE}
| "pointillé" {POINTILLE}
| "pointillés" {POINTILLE}
| "point" {POINT}
| "points" {POINT}
| "droite" {DROITE}
| "segment" {SEGMENT}
| "vecteur" {VECTEUR}
| "repere" {REPERE}
| "repère" {REPERE}
| "cercle" {CERCLE}
| "longueur" {LONGUEUR}
| "quelconque" {QUELCONQUE}
| "quelconques" {QUELCONQUE}
| "intersection" {INTERSECTION}
| "intersections" {INTERSECTION}
| "milieu" {MILIEU}
| "médiatrice" {MEDIATRICE}
| "mediatrice" {MEDIATRICE}
| "bissectrice" {BISSECTRICE}
| "perpendiculaire" {PERPENDICULAIRE}
| "parallèle" {PARALLELE}
| "parallele" {PARALLELE}

| "deux" {INTEGER(2)}
| "trois" {INTEGER(3)}
| "quatre" {INTEGER(4)}
| "cinq" {INTEGER(5)}
| "six" {INTEGER(6)}
| "sept" {INTEGER(7)}
| "huit" {INTEGER(8)}
| "neuf" {INTEGER(9)}
| "dix" {INTEGER(10)}

(* les couleurs... *) 
| "rouge" {ROUGE}
| "vert" {VERT}
| "bleu" {BLEU}
| "noir" {NOIR}
| "orange" {ORANGE}
| "jaune" {JAUNE}
| "rose" {ROSE}
| "blanc" {BLANC}
| "gris" {GRIS}
| "violet" {VIOLET}

| "et" {ET}
| "à" {A}
| "a" {A}
| "passant" {PASSANT}
| "par" {PAR}
| "centre" {CENTRE}
| "sur" {SUR}

| '.' {EOL}
| ';' {EOL}
| ',' {VIRGULE}
| "/" {SLASH}
| '[' {CROCHET_OUV}
| ']' {CROCHET_FER}
| '(' {PARENT_OUV}
| ')' {PARENT_FER}
| '|' {NORME}

| ident {IDENT (lexeme lexbuf)}
| digit+ {INTEGER (int_of_string (lexeme lexbuf))}

| _ {raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }
| eof {EOF}


and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }
  | _    { comment lexbuf }


