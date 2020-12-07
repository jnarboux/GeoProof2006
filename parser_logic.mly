%{

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

open Formulas
open Fol
 
%}

%token LPAR RPAR
%token IMP IFF EQ OR AND NOT TRUE FALSE COMMA
%token <string> IDENT
%token EOL EOF

%nonassoc IFF
%right IMP 
%nonassoc EQ 
%left OR 
%left AND
%nonassoc NOT


%start start
%type <Fol.fol Formulas.formula> start
%%

list:
     | IDENT COMMA list {Var($1)::$3}
     | IDENT            {[Var($1)]}
;

expr:
     | TRUE 		   { True }
     | FALSE 		   { False }
     | LPAR expr RPAR      { $2 }
     | IDENT LPAR list RPAR{ Atom(R($1,$3)) }
     | expr IFF expr       { Iff($1,$3) }
     | expr AND expr       { And($1,$3) }
     | expr OR expr        { Or ($1,$3) }
     | expr IMP expr       { Imp($1,$3) }
     | NOT expr            { Not($2) }	 
     | IDENT EQ IDENT      { Atom(R("=",[Var($1);Var($3)])) }
;

start: expr EOF { $1 }
;
