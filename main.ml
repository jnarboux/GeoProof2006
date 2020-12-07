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

open I18n
open Optionsdeconfiguration

let version = ref false
let author = ref false
let licence = ref false

let options_list =
  [
   ("--version", Arg.Set version,(i18n  "Display the version number"));
   ("-v", Arg.Set version, (i18n "Display the version number"));
   ("--authors",  Arg.Set author, (i18n "List authors"));
   ("--licence",  Arg.Set licence, (i18n "Display licence"));
   ("--lang",  Arg.String I18n.choose_lang, (i18n ("Set the language. Available languages are :"^(List.fold_left(fun s t -> s^" "^t) "" Autoconf.languages))))
 ]

let use_message = (i18n "Usage : geoproof [options] file.drg, use --help for help")

let file = ref None

let supported_file_extensions = [".geo";".zir";".kig";".drg";".con.xml";".cst"]

let set_file s =  
  if not (List.exists (Filename.check_suffix s) supported_file_extensions) then 
    raise (Arg.Bad "unknown extension");
  file := Some s

(* Charge les options depuis le fichier config.ini *)
let charge_options() =
  try 
    Options.load Optionsdeconfiguration.config_ini;
    Debug.pdb "Options chargées"
  with
    _ -> 
      begin
        print_endline ((i18n "The configuration file ")^config_name^(i18n " can not be opened."));
        print_endline (i18n "Creating configuration file.");
  	if not (Sys.file_exists config_dir) then
	  begin 
            print_endline ((i18n "Creating the directory .")^Autoconf.name^(i18n "."));
	    Unix.mkdir config_dir 0o750
	  end;
        try
          Options.save_with_help Optionsdeconfiguration.config_ini
        with
          _ -> print_endline (i18n "The configuration file could not be saved.")
      end

let print_version () =
  print_endline ((i18n "This is GeoProof Version :")^Autoconf.version);
  print_endline ((i18n "./configure.sh was run on :")^Autoconf.compiled);
  print_endline ((i18n "Ostype :")^Sys.os_type)

let print_author () =
  print_endline (Autoconf.name^(i18n " was developed by :"));
  print_endline ("Nicolas François "^(i18n "and")^" Julien Narboux");
  print_endline (i18n "Please use http://gna.org/bugs/?group=geoproof to report bugs.")

let print_licence () =  
  print_endline (Autoconf.name^(i18n " is distributed under GNU General Public Licence Version 2"))

let main () = 
  Arg.parse options_list set_file use_message;
  (match !file with 
    Some f ->   
      if Filename.check_suffix f ".geo" then
	Input_langue_naturelle.entree_langue_naturelle f
      else if  
	List.exists (Filename.check_suffix f) supported_file_extensions
      then
	Input_xml.entree_xml f
      else assert false (* The extension has already been checked *)
  | None -> ());

  if !version then print_version ();
  if !author then print_author ();
  if !licence then print_licence ();
  
  if (not (!version || !author || !licence))  then
    begin
      charge_options();	
      Automate.initialise()
    end

let _ = main () 
