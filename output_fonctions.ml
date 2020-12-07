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
 
open I18n

(* This fonctions write in a file with the function f. 
   if overwrite is true, we overwrite the file without asking
   otherwise we ask what should be done.

   Files are written using -rw-r--r-- file permission.
 *)

let write_file_aux file f =
  try 
    let c = open_out_gen [Open_creat;Open_trunc;Open_wronly] 0o644 file in
    f c;
    flush c;
    close_out c
  with 
    e -> 
      GToolbox.message_box 
	~title:((i18n "Error trying to write file ")^file) 
	((i18n "Error trying to write file ")^file^" :\n"^(Printexc.to_string e))

let write_file  ?(overwrite=false) file f =  
  if overwrite then
    write_file_aux file f 
  else
    if Sys.file_exists file then
      begin
	let answer =
	  GToolbox.question_box 
	    ~title:((i18n "Do you want to overwrite file ")^file^" ?") 
	    ~buttons:[(i18n "Overwrite");(i18n "Cancel")]
	    ~default:2
	    (i18n "File "^file^" already exists.\nDo you want to overwrite it ?")
	in
	match answer with
	  1 -> 
	    begin
	      try 
		write_file_aux file f 
	      with 
		e -> 
		  GToolbox.message_box 
		    ~title:((i18n "Error trying to write file ")^file) 
		    ((i18n "Error trying to write file ")^file^" :\n"^(Printexc.to_string e))	  
	    end
	| 2 -> ()
	| _ -> assert false	
      end
    else
      begin
	write_file_aux file f 
      end

(*	
let to_file ?(overwrite=false) f file = 
  let fonc c = Construction.faire_pour_tout_objet 
      (fun o -> Printf.fprintf c "%s\n" (f o ()))
  in
  write_file ~overwrite file fonc
  *)  
     	
let to_string f () =
  let s = ref "" in
  Construction.iter
    (fun o -> s := !s^"\n"^(f o ()));
  !s
