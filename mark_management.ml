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

open Types_de_base
open Objets_graphiques
open Repere
open Geometric_functions.GeometryFunctions

(* This file contains the code to manage the marks 
   (segment marks or angle marks). 
   When a mark is added it computes the symbol 
   which should be used. *)

let get_mark_style o =
  match o#objet_style with
    STYLE_MARK(x)-> x
  | _ -> assert false
	
(* Computes the list of marks associated to an object s which are not obj_nom *)
	
let get_other_marks s obj_nom =
  let f o = o#objet_nom <> obj_nom && o#objet_type = TYPE_MARK && List.mem s o#dependance in
  Construction.filter f
    
(* Intersection of two lists *)
    
let inter l1 l2 =  List.filter (fun x -> List.mem x l1) l2 	
      
let smallest m1 m2 = 
  match m1,m2 with
    LINE(n),LINE(m) 
  | CROSS(n),CROSS(m)-> if n < m then m1 else m2 
  | LINE(n),CROSS(m) -> m1 
  | CROSS(n),LINE(m) -> m2 


let bigest m1 m2 = 
  match m1,m2 with
    LINE(n),LINE(m) 
  | CROSS(n),CROSS(m)-> if n > m then m1 else m2 
  | LINE(n),CROSS(m) -> m2 
  | CROSS(n),LINE(m) -> m1 
  
let next_mark m =
  match m with
    LINE(n) -> LINE(n+1)
  | CROSS(n) -> CROSS(n+1)
	
(* TODO decide of an order *)

let smallest_available_mark obj () = 
  let max = ref (LINE(0)) in
  let aux o =
    if o#objet_type = TYPE_MARK then   
      begin
	let m1 = get_mark_style o in
	  (match m1 with LINE(n) -> Debug.pdb (string_of_int n));
	  max := bigest m1 !max
      end
  in
    Construction.iter_before obj aux;
    next_mark !max
  
(* Given two segments computes the mark which should be used *)
(* and in some cases modify the existing marks *)    

let compute_mark obj s1 s2 () =
  Debug.pdb "computemark";
  match (get_other_marks s1 obj, get_other_marks s2 obj) with
    [],[] -> Debug.pdb "smallest";smallest_available_mark obj ()
  | l,[] | [],l -> Debug.pdb "a";get_mark_style (List.hd l)
  | [x],[y] -> Debug.pdb "b";
      let sx = get_mark_style x in
      let sy = get_mark_style y in
      if sx = sy then
	smallest_available_mark obj ()
      else
	smallest sx sy	
  | l1,l2 -> Debug.pdb "c";
      (
      match inter l1 l2 with
 	[] ->  smallest_available_mark obj ()
      | l -> get_mark_style (List.hd l)
      )
    
let draw_mark (win:GDraw.pixmap) rep m s =
  let c1 = s#coordonnees_premier_point in
  let c2 = s#coordonnees_second_point in
  let pixel n =
    let o = origine_ecran_t rep in
    let v = ecran_vers_papier {x=o.x;y=o.y +! (of_int n)} () in
      norm v
  in
  let draw_one_mark i =
    let v = v_minus c2 c1 in
    let g = v_normalized (v_plus (v_scalar (of_int 2) (v_norm v)) v) in
    let h = v_scalar (pixel 7) g in 
    let a = v_plus i h in
    let b = v_minus i h in     
    let cb = papier_vers_ecran rep b in
    let ca = papier_vers_ecran rep a in
      win#line ca.xint ca.yint cb.xint cb.yint 
  in
  let iplusn n = 
    let i = mid_point c1 c2 in
    let v = v_normalized (v_minus c2 c1) in
    let vn = v_scalar (pixel n) v in
      v_plus i vn
  in
  let iminusn n = 
    let i = mid_point c1 c2 in
    let v = v_normalized (v_minus c2 c1) in
    let vn = v_scalar (pixel n) v in
      v_minus i vn
  in
    match m with 
	LINE(0) -> ()
      | LINE(1) -> draw_one_mark (iplusn 0)
      | LINE(2) -> List.iter draw_one_mark [iplusn 2;iminusn 2]
      | LINE(3) -> List.iter draw_one_mark [iplusn 4;iminusn 4;iplusn 0]
      | LINE(4) -> List.iter draw_one_mark [iplusn 6;iplusn 2;iminusn 2;iminusn 6]
      | LINE(n) ->  
	  draw_one_mark (iplusn 0);
	  let pos =  papier_vers_ecran rep (mid_point c1 c2) in
	  Font.draw win (string_of_int n) pos.xint pos.yint
      | _ -> failwith "not implemented" 
	  
    

