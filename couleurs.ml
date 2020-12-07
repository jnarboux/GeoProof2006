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

let white = `WHITE
let black = `BLACK
let red =    `NAME "red"
let green =  `NAME "green"
let yellow = `NAME "yellow"
let blue =   `NAME "blue"
let orange = `NAME "orange"
let purple = `NAME "purple"
let grey =   `NAME "grey"
let pink =   `NAME "pink"


let light_color c = match c with  
|  `NAME "red" -> `RGB(65000,40000,40000)
|  `NAME "green" -> `RGB(55000,65000,55000)
|  `NAME "yellow" ->  `RGB(65000,65000,35000)
|  `NAME "blue" ->  `RGB(40000,40000,65000)
|  `NAME "orange" ->  `RGB(65000,50000,50000)
|  `NAME "purple" ->  `RGB(65000,40000,65000)
|  `NAME "grey" ->  `RGB(60000,60000,60000)
|  `NAME "pink" ->  `RGB(65000,60000,60000)
| `BLACK ->  `NAME "grey"
| `WHITE ->  `NAME "grey"
| _ -> assert false


let string_of_color c =
  match c with
  | `NAME col -> col
  | `BLACK ->  "black"
  | `WHITE ->  "white"
  | _ -> assert false

	
let rgb_of_color c =
  match c with
  |  `NAME "red" -> 255,0,0
  |  `NAME "green" -> 0,255,0
  |  `NAME "yellow" -> 255,255,0
  |  `NAME "blue" ->  0,0,255
  |  `NAME "orange" ->  255,155,30
  |  `NAME "purple" ->  255,0,255
  |  `NAME "grey" ->  128,128,128
  |  `NAME "pink" ->  255,156,197
  | `BLACK ->  0,0,0
  | `WHITE ->  255,255,255
  | _ -> assert false

let eukleides_color_of_color c =
  match c with
    |  `NAME "red" -> "red"
    |  `NAME "green" -> "green"
    |  `NAME "yellow" ->  "yellow"
    |  `NAME "blue" ->  "blue"
    |  `NAME "orange" ->  "magenta"
    |  `NAME "purple" ->  "cyan"
    |  `NAME "grey" ->  "gray"
    |  `NAME "pink" ->  "darkgray"
    | `BLACK ->  "black"
    | `WHITE ->  "white"
    | _ -> assert false
 
let color_of_rgb r g b =
  match (r,g,b) with
    | 255,0,0 ->  `NAME "red" 
    |  0,255,0-> `NAME "green"
    | 255,255,0-> `NAME "yellow"
    | 0,0,255-> `NAME "blue"
    | 255,155,30-> `NAME "orange"
    | 255,0,255 -> `NAME "purple"
    | 128,128,128 ->  `NAME "grey"
    | 255,156,197 ->  `NAME "pink"
    | 0,0,0 -> `BLACK
    | 255,255,255 -> `WHITE
    | _ -> `BLACK
