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

open Types_de_base
open Geometric_functions.GeometryFunctions
open Repere

let distance_vers_papier rep z = (of_int z) /! rep.d.x
let distance_vers_ecran rep z = to_int (z *! rep.d.x)

let distance_vers_ecran_t rep z = z *! rep.d.x

let distance2_vers_ecran rep z2 =
  let delta = rep.d.x in
    to_int (z2 *! delta *! delta)

let unpack x = match x with 
    Some(a) -> a 
  |_ -> assert false 

(*
let coin_sud_ouest () =
  ecran_vers_papier {x = of_int 0; y = of_int 0} ()
let coin_nord_est () =
  ecran_vers_papier {x = !screen_width; 
		     y = !screen_height} ()


let x_min_ecran () = (coin_sud_ouest ()).x
let x_max_ecran () = (coin_nord_est()).x
let y_min_ecran () = (coin_sud_ouest()).y
let y_max_ecran () = (coin_nord_est()).y
*)

 
  
