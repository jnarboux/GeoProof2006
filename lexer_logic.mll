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

{
   open Parser_logic
   exception Eof
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let space = [' ' '\t' '\n']
let ident = alpha ('_'|alpha|digit)*

rule token = parse
    space+         { token lexbuf }     (* skip blanks *)
  | "==>"          { IMP }
  | "<=>"          { IFF }
  | '~'            { NOT }
  | "\\/"          { OR }
  | "/\\"          { AND }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | "="            { EQ }
  | '('            { LPAR }
  | ')'            { RPAR }
  | ','            { COMMA }
  | ident as id    { IDENT(id) }
  | eof            { EOF }
