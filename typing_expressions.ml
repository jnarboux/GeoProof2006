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

type the_type = Tfloat | Tbool 

let funs = Hashtbl.create 97
  
let () =
  Hashtbl.add funs "cos" ([Tfloat], Tfloat);
  Hashtbl.add funs "sin" ([Tfloat], Tfloat);
  Hashtbl.add funs "tan" ([Tfloat], Tfloat);
  Hashtbl.add funs "arccos" ([Tfloat], Tfloat);
  Hashtbl.add funs "arcsin" ([Tfloat], Tfloat);
  Hashtbl.add funs "arctan" ([Tfloat], Tfloat);
  Hashtbl.add funs "sqrt" ([Tfloat], Tfloat);
  Hashtbl.add funs "abs" ([Tfloat], Tfloat);
  Hashtbl.add funs "min" ([Tfloat;Tfloat], Tfloat);
  Hashtbl.add funs "max" ([Tfloat;Tfloat], Tfloat);

(* TODO sinh cosh tanh round floor ceil random sign... *)

exception ExpectedType of string * the_type * string * the_type
exception UnboundVar of string


(*
let rec type_expr env expr =
  match expr with
    | Binop(op,e1,e2) ->
	let te1 = type_expr env e1 in
	let te2 = type_expr env e2 in
	  begin match op with
              Plus | Mult | Minus | Div| Power -> 
		if te1 <> Tfloat then raise (ExpectedType("",te1,"",Tfloat));
		if te2 <> Tfloat then raise (ExpectedType("",te2,"",Tfloat));
		Tfloat
	    | Lt | Gt | Le | Ge | Eq | Or | And ->
		Tbool
	  end
    | Unop(op,e) ->
	let te  = type_expr env e in
	  Tfloat
*)

let dependency expr =
  let rec dependency_aux env expr =
    match expr with
	Cst(_) | CString(_) | BCst(_) -> []
      | Var(s) -> 
	  if not (List.mem s env) then
            try 
	      [Construction.get_object s]
	    with
	      | _ -> []
	  else []
      | Binop(b,e1,e2) ->
	  let e1 = dependency_aux env e1 in
	  let e2 = dependency_aux env e2 in
	    e1@e2
      | Unop(u,e) ->
	  dependency_aux env e
      | Letin(x,e1,e2) ->
          let e1 = dependency_aux env e1 in
	  let e2 = dependency_aux (x::env) e2 in
	    e1@e2
      | If(e1,e2,e3) ->
	  let e1 = dependency_aux env e1 in
	  let e2 = dependency_aux env e2 in
	  let e3 = dependency_aux env e3 in
	    e1@e2@e3
      | FunApp(s,args) ->
	  List.flatten (List.map (dependency_aux env) args)
  in dependency_aux [] expr
