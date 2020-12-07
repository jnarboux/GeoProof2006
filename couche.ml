
(* $Id: couche.ml,v 1.6 2005/05/06 09:10:28 gmariano Exp $ *)

open Options
open Optionsdeconfiguration

(*MG open I18n *)


type attribut_couche = 
    { 
      mutable cache : bool;
      mutable nom : string;
      mutable couleur : GDraw.color option
    }

let  une_couche_par_defaut i = {
  cache=false;
  nom=((I18n.i18n "Layer ")^(string_of_int (i+1)));
  couleur=None
}
  
let couches = Array.init !!nombre_de_couches une_couche_par_defaut    
  
    
(*

 GeoProof, an interactive geometry tool writen in OCaml.                   
 Copyright (C) 2004 Nicolas François et Julien Narboux                      
                                                                            
 This program is free software; you can redistribute it and/or              
 modify it under the terms of the GNU General Public License                
 as published by the Free Software Foundation; either version 2             
 of the License, or (at your option) any later version.                     
                                                                            
 This program is distributed in the hope that it will be useful,            
 but WITHOUT ANY WARRANTY; without even the implied warranty of             
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              
 GNU General Public License for more details.                               
                                                                            
 You should have received a copy of the GNU General Public License          
 along with this program; if not, write to the Free Software                
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)
  




