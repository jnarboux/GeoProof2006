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

type expr =
    Cst of string (* The number may be using in any precision 
		     so we use string to represent them *)
  | CString of string
  | BCst of bool
  | Var of ident
  | Binop of binop*expr*expr
  | Unop of unop*expr
  | If of expr*expr*expr
  | Letin of ident*expr*expr
  | FunApp of ident*(expr list)

and binop = Plus | Mult | Minus | Div| Power | Lt | Gt | Le | Ge | Eq | Or | And

and unop = Neg | Not

and ident = string
