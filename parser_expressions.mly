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

  open Ast_expressions
 
%}

%token LPAR RPAR COMMA
%token LET IN  
%token OR AND NOT TRUE FALSE
%token <string> IDENT
%token <string> CST
%token <string> STRING
%token <string> ANYTHING
%token EOL EOF
%token PLUS MULT MINUS DIV POWER
%token IF THEN ELSE
%token LT GT LE GE EQ NEQ E PI

%left  LET IN
%right IF ELSE
%left OR
%left AND
%nonassoc NOT
%nonassoc LT GT LE GE EQ NEQ
%left PLUS MINUS
%left MULT DIV
%nonassoc uminus
%left POWER

%start start
%type <Ast_expressions.expr> start
%%

 list:
| expr COMMA list             { $1::$3 }
| expr                        { [$1] }
  ;
 
  expr: 
| CST                         { Cst($1) }
| STRING                      { CString($1) }
| E                           { FunApp("e",[]) }
| PI                          { FunApp("pi",[]) }
| IDENT                       { Var($1) }
| TRUE                        { BCst(true) }
| FALSE                       { BCst(false) }
| LPAR expr RPAR              { $2 }

| IDENT LPAR list RPAR        { FunApp($1,$3) } 

| LET IDENT EQ expr IN expr   { Letin ($2,$4,$6) }
| IF expr THEN expr ELSE expr { If($2,$4,$6) }
      
    /* Boolean operators */
| expr AND expr               { Binop(And,$1,$3) }
| expr OR expr                { Binop(Or,$1,$3) }
      
| NOT expr                    { Unop(Not,$2) }
	  
	/* Comparison operators */
| expr LT expr                { Binop(Lt,$1,$3) }
| expr GT expr                { Binop(Gt,$1,$3) }
| expr LE expr                { Binop(Le,$1,$3) }
| expr GE expr                { Binop(Ge,$1,$3) }
| expr EQ expr                { Binop(Eq,$1,$3) }
| expr NEQ expr               { Unop(Not,Binop(Eq,$1,$3)) }
      
    /* Arithmetic operators */
| expr MULT expr              { Binop(Mult,$1,$3) }
| expr DIV expr               { Binop(Div,$1,$3) }
| expr PLUS expr              { Binop(Plus,$1,$3) }
| expr MINUS expr             { Binop(Minus,$1,$3) }
| expr POWER expr             { Binop(Power,$1,$3) }
| MINUS expr %prec uminus     { Unop(Neg,$2) }	 
  ;
  
  start: 
| expr EOF                    { $1 }
| EOF                         { CString("") }
  ;
