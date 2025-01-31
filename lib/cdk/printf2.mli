(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)


val cprintf : (string -> unit) -> ('a, unit, unit) format -> 'a
(** [cprintf k format arguments] is the same as [printf format arguments],
    except that the resulting string is passed as argument to [k]; the
    result of [k] is then returned as the result of [cprintf]. *)

val lprintf :  ('a, unit, unit) format -> 'a
val lprintf_nl :  ('a, unit, unit) format -> 'a
val lprint_newline : unit -> unit  
val lprint_char : char -> unit  
val lprint_string : string -> unit  
val lprint_int : int -> unit  

(*
val lprintf_to_stdout : bool ref  
val lprintf_fifo : string Fifo.t
val lprintf_max_size : int ref
val lprintf_output : out_channel option ref
val set_lprintf_handler : (string -> unit) -> unit
*)

val lprintf_max_size : int ref
  
val detach : unit -> unit
val log_to_file : out_channel -> unit
val log_to_buffer : Buffer.t -> unit
val set_logging : bool -> unit
val close_log : unit -> unit
