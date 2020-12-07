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

(*
open GuiGettext.Gettext

let init = Gettext.init
*)

(*
open Camlgettext
*)

open Debug

(*
let codeset =  (snd (Glib.Convert.get_charset ()))
let _ = pdb "Your codeset is"
let _ = pdb codeset
*)

let choose_lang s = ();;
(*
  if List.mem s Autoconf.languages then
    begin
      pdb s;
      Unix.putenv "LANGUAGE" s;
      pdb (setlocale LC_ALL "")
    end
  else 
    begin
      print_endline "The language specified is not a valid language, please choose among :";
      List.iter (fun s -> print_endline ("- "^s))
	Autoconf.languages
    end

*)

(* On demande directement à gettext de renvoyer de l'utf8, 
car c'est ce que demande lablgtk *)

(*
let _ = bind_textdomain_codeset Autoconf.name "utf8"
*)

(*
let _ = Camlgettext.textdomain Autoconf.name 
let _ = Camlgettext.bindtextdomain Autoconf.name "./locale/" 
*)
let utf8 s = Glib.Convert.locale_to_utf8 s
(*
 (Glib.Convert.convert_with_fallback ~fallback:"X" 
     ~to_codeset:"utf8"  ~from_codeset:"ISO-8859-1" s)
*)

let i18n s = utf8 (s)

(*
  (utf8 (gettext s))
*)
