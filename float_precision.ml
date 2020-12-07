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

type t = float
let zero = 0.
let one = 1.
let two = 2. 
let pi = 3.141592653589793
let e =  2.718281828459045

let exp = exp
let pow = ( ** )

let ln = log
let log b x = (log x) /. (log b) 

let compare _ x y = if x < y then 1 else if x>y then -1 else 0
let (+!) = (+.)
let (-!) = (-.)
let (/!) = (/.)
let ( *! ) = ( *. )
let neg = (fun x -> -. x)
let sqrt = sqrt
let abs = abs_float
let min = min
let max = max

let of_int = float_of_int
let of_float = fun x -> x
let of_string = float_of_string

let to_int f = int_of_float (f +. 0.5)
let to_float x i = x
let to_string f _ = Printf.sprintf "%.2f" f

let sin = sin
let cos = cos
let tan = tan
  
let arcsin = asin
let arccos = acos
let arctan = atan
