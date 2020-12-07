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

{
  open Lexing
  open Parser_expressions
   exception Eof
   exception Unterminated_string of int
   exception Unterminated_expr of int
   exception Illegal_control_sequence of int * string
 
   let string_start = ref (-1)
   let expr_start = ref (0)

   let string_buffer = Buffer.create 80
   let expr_buffer = Buffer.create 80
   
   let start_string lb =
     string_start := lexeme_start lb
    
   let start_expr lb =
     expr_start := lexeme_start lb
       
   let unterminated_string () =
     assert (!string_start >= 0);
     raise (Unterminated_string !string_start)

   let unterminated_expr () =
     raise (Unterminated_expr !expr_start)
     
   let illegal_control_sequence lb =
     raise (Illegal_control_sequence (lexeme_start lb - 1, lexeme lb))

   let keyword_list = [
     "if", IF;
     "then", THEN;
     "else", ELSE;
     "true", TRUE;
     "false", FALSE;
     "not", NOT;
     "let", LET;
     "in", IN;
     "or", OR;
     "and", AND;
     "e" , E;
     "pi" , PI
   ]
     
   let keyword_table =
     let t = Hashtbl.create 73 in
       List.iter (fun (name,kwd) -> Hashtbl.add t name kwd) keyword_list;
       t
	 
   let id_or_kwd s =  try Hashtbl.find keyword_table s with _ -> IDENT(s)

}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let space = [' ' '\t' '\n']
let ident = alpha ('_'|alpha|digit)*
let float = integer? '.' ['0'-'9']* (['e''E']['+''-']integer)?
let number = float | integer

rule expr = parse
    space+         { expr lexbuf }     (* skip blanks *)
  | ident          { id_or_kwd (Lexing.lexeme lexbuf) } 
  | '('            { LPAR }
  | ')'            { RPAR } 
  | ','            { COMMA }
(*  | '#'            { Debug.pdb "closing#";anything lexbuf } *)
  | '"'            { start_string lexbuf;
		     let s = string lexbuf in
		       lexbuf.lex_start_pos <- !string_start - lexbuf.lex_abs_pos;
		       STRING s }

  (* Arithmetic operators *)
  | '*'            { MULT }
  | '-'            { MINUS }
  | '/'            { DIV }
  | '+'            { PLUS } 
  | '^'            { POWER }

  (* Comparison operators *)
  | "<>"           { NEQ }
  | '<'            { LT }
  | '>'            { GT }
  | "<="           { LE }
  | ">="           { GE }
  | "="            { EQ }

  | number as f    { CST(f) }
  | eof            { EOF }


and string = parse
  | '"' { let s = Buffer.contents string_buffer in
	    Buffer.clear string_buffer;
	    s}

  | eof { unterminated_string () }
  | "\\\\" { Buffer.add_char string_buffer '\\'; string lexbuf }
  | "\\n" { Buffer.add_char string_buffer '\n'; string lexbuf }
  | "\\t" { Buffer.add_char string_buffer '\t'; string lexbuf }
  | "\\\"" { Buffer.add_char string_buffer '"'; string lexbuf }
  | '\\' _ { illegal_control_sequence lexbuf }
  | _ { Buffer.add_string string_buffer (lexeme lexbuf); string lexbuf }


