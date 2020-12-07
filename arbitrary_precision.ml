
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
open Options
open Optionsdeconfiguration

open Creal
exception No_Solution 
type t = Creal.t
let zero = zero
let one = one
let two = two 
let pi = pi
let e = e

let compare p x y = rel_cmp p y x
let (+!) = add
let (-!) = sub
let (/!) = 
  fun x -> fun y -> 
    if rel_cmp !!precision y zero = 0 then raise No_Solution; 
    (div x y)  (* TODO *)
let ( *! ) = mul
let neg = neg
let sqrt = sqrt
let abs = abs
let min = min
let max = max
  
let exp = exp
let pow = pow
  
let ln = ln
let log = log
  
let of_int = of_int
let of_float = of_float
let of_string s = of_string s 10 (* we use only base 10 *)
  
let to_int x = int_of_float ((to_float x !!precision) +. 0.5)
let to_float = to_float 
let to_string = to_string
  
let sin = sin
let cos = cos
let tan = tan
  
let arcsin = arcsin
let arccos = arccos
let arctan = arctan
