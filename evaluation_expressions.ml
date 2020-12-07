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
open Ast_expressions
open Geometric_functions.GeometryFunctions
 
exception VarUndef of string

type result =  
    Bool of bool 
  | String of string 
  | Number of Geometric_functions.GeometryFunctions.t
  | Object of Objets_graphiques.objet_graphique

let app_bin_num f x y = 
  match x,y with
      Number a,Number b -> Number(f a b)
    | _ -> assert false

let app_bin_bool f x y = 
  match x,y with
      Bool a,Bool b -> Bool(f a b)
    | _ -> assert false 
	
let app_bin_num_bool f x y = 
  match x,y with
     Number a,Number b -> Bool(f a b)
    | _ -> assert false 


let equal x y =
  match x,y with
      Bool a,Bool b -> Bool(a=b)
    | Number a,Number b -> Bool(a=b)
    | String a,String b -> Bool(a=b)
    | _ -> assert false 
	
let negation x = match x with
    Bool b -> Bool(not b)
  | _  -> assert false

let opposite x = match x with
    Number a -> Number(neg a)
  | _  -> assert false
      
let function_of_binop = function
    Plus -> app_bin_num (+!) 
  | Mult -> app_bin_num ( *!)
  | Minus -> app_bin_num (-!)
  | Div -> app_bin_num (/!)
  | Power ->  app_bin_num ( **! ) 
  | Lt  -> app_bin_num_bool (<!)
  | Gt -> app_bin_num_bool (>!)
  | Le ->  app_bin_num_bool (<=!)
  | Ge  ->   app_bin_num_bool (>=!)
  | Or ->  app_bin_bool (||)
  | And ->  app_bin_bool (&&)
  | Eq  -> app_bin_num_bool (=!)
   
let function_of_unop = function
    Neg -> opposite
  | Not -> negation 

let app1 f l = 
  match l with
      [Number x] -> Number(f x)
    | _ -> assert false

let app2 f l =
  match l with 
      [Number x;Number y] -> Number(f x y)
    | _ -> assert false

let app3pn f l =
  match l with 
      [Object a;Object b;Object c] -> 
	let a = a#coordonnees_point in
	let b = b#coordonnees_point in
	let c = c#coordonnees_point in
	  Number(f a b c)
    | _ -> assert false

let app2pn f l =
  match l with 
      [Object a;Object b] -> 
	let a = a#coordonnees_point in
	let b = b#coordonnees_point in
	  Number(f a b)
    | _ -> assert false

let app2pb f l =
  match l with 
      [Object a;Object b] -> 
	let a = a#coordonnees_point in
	let b = b#coordonnees_point in
	  Bool(f a b)
    | _ -> assert false

let app2lb f l =
  match l with 
      [Object a;Object b] -> 
	let a = a#equation_droite in
	let b = b#equation_droite in
	  Bool(f a b)
    | _ -> assert false

let app3pb f l =
  match l with 
      [Object a;Object b;Object c] -> 
	let a = a#coordonnees_point in
	let b = b#coordonnees_point in
	let c = c#coordonnees_point in
	  Bool(f a b c)
    | _ -> assert false

let app2sb f l =
  match l with 
      [Object s1;Object s2] -> 
	let a = s1#coordonnees_premier_point in
	let b = s1#coordonnees_second_point in
	let c = s2#coordonnees_premier_point in
	let d = s2#coordonnees_second_point in
	  Bool(f a b c d)
    | _ -> assert false

let  app6pb g l =
  match l with 
      [Object a;Object b;Object c;Object d;Object e;Object f] -> 
	let a = a#coordonnees_point in
	let b = b#coordonnees_point in
	let c = c#coordonnees_point in
	let d = d#coordonnees_point in
	let e = e#coordonnees_point in
	let f = f#coordonnees_point in
	  Bool(g a b c d e f)
    | _ -> assert false



let apply_fun s args_values =
  match s with
      "cos" -> app1 cos args_values
    | "sin" -> app1 sin args_values
    | "tan" -> app1 tan args_values
    | "arccos" -> app1 arccos args_values
    | "arcsin" -> app1 arcsin args_values
    | "arctan" -> app1 arctan args_values
    | "ln" ->  app1 ln args_values
    | "exp" ->  app1 exp args_values
    | "log" ->  app2 log args_values
    | "pow" ->  app2 pow args_values
    | "sqrt" -> app1 sqrt args_values
    | "abs" -> app1 abs args_values
    | "min" -> app2 min args_values
    | "max" -> app2 max args_values
    | "e" -> (Number e)
    | "pi" -> (Number pi)
    | "area" -> app3pn area args_values
    | "signed_area" -> app3pn signed_area args_values
    | "angle" -> app3pn angle args_values
    | "length" -> app2pn distance args_values
    | "eq_points" -> app2pb equals_point args_values
    | "collinear" -> app3pb collinear args_values
    | "left_turn" -> app3pb left_turn args_values
    | "between" -> app3pb between args_values
    | "eq_lengths" -> app2sb congruent_segments args_values
    | "eq_angles" -> app6pb congruent_angles args_values
    | "parallel" -> app2lb are_parallel args_values
    | "orthogonal" -> app2lb are_orthogonal args_values
    | _ -> assert false
	
    
let eval =
  let rec eval_rec env expr =
    match expr with
	Cst(s) -> Number(of_string s)
      | CString(s) -> String(s)
      | BCst(b) -> Bool(b)
      | Var(s) -> (try List.assoc s env with
                       Not_found -> 
			 (try 
			   Object (Construction.get_object s);
			 with
			 | _ -> raise(VarUndef s)))				 
	  
      | Binop(b,e1,e2) ->
	  let e1 = eval_rec env e1 in
	  let e2 = eval_rec env e2 in
	    (function_of_binop b) e1 e2
      | Unop(u,e) -> 
	  let e = eval_rec env e in
	    (function_of_unop u) e
      | Letin(x,e1,e2) ->
          let t = eval_rec env e1 in 
	    eval_rec ((x,t)::env) e2 
      | If(e1,e2,e3) ->
	  let c = eval_rec env e1 in
	    begin
	      match c with
		  Bool(true) -> eval_rec env e2
		| Bool(false) -> eval_rec env e3
		| _ -> assert false
	    end
      | FunApp(s,args) ->
	  (* We implement call by value *)
	  let args_values = 
	    List.map (eval_rec env) args
	  in
	    apply_fun s args_values
  in
    eval_rec []

let string_of_result x = 
  match x with
      Bool true -> i18n("true")
    | Bool false -> i18n("false")
    | Number x  -> to_string x 10 (* TODO precision *)
    | String s -> s
    | Object o -> o#objet_nom	  


let eval_one_expr s =
  let lexbuf = Lexing.from_string s in
  let expr = Parser_expressions.start Lexer_expressions.expr lexbuf in 
  let value = 
    try eval expr with 
      | VarUndef s ->  String("Error "^s^" is unknown")
      | _ -> String("Error")
  in
  let r = string_of_result value in
    r 

type command =  Meta of string | Expr of string

let extract_expressions s =
  let rec extract_expressions_aux s i = 
    if  String.contains_from s i '#' then
      begin
	let st = (String.index_from s i '#') + 1 in
	let ed = (String.index_from s st '#') - 1 in
	let len = ed - st + 1 in
	let x = 
	  if st <> i 
	  then
	    [Meta(String.sub s i (st-i-1));Expr(String.sub s st len)]
	  else
	    [Expr(String.sub s st len)]
	in 
	  x@(extract_expressions_aux s (ed+2))
      end
    else
      let len = (String.length s) - i in
	[(Meta (String.sub s i len))]
  in
    extract_expressions_aux s 0

let rec dependancies e = 
  match e with
      Cst _ | CString _ | BCst _ -> []
    | Var i -> [Construction.get_object i]
    | Binop(_,e1,e2) | Letin(_,e1,e2) -> (dependancies e1) @ (dependancies e2)
    | Unop(_,e) -> (dependancies e)
    | If(e1,e2,e3) -> (dependancies e1) @ (dependancies e2) @ (dependancies e3)
    | FunApp(_,l) -> List.flatten (List.map dependancies l)

let dependancies_string s =
  try
    let l = extract_expressions s in 
    let process_command c = 
      match c with
	  Meta s -> []
	| Expr s ->   
	    let lexbuf = Lexing.from_string s in
	    let expr = Parser_expressions.start Lexer_expressions.expr lexbuf in 
	      dependancies expr
    in
      List.flatten (List.map process_command l)
  with
    | _ -> failwith "compute_dependancies : Error"
	
let eval_string s = 
  try
    let l = extract_expressions s in 
    let process_command c = 
      match c with
	  Meta s -> s
	| Expr s -> eval_one_expr s
    in
    let s = String.concat "" (List.map process_command l) in 
      s
  with
    | _ -> "Error"

let collect_predicates s =
  let pred_list =
    ["eq_points";
     "collinear";
     "left_turn";
     "between";
     "eq_lengths";
     "eq_angles";
     "parallel";
     "orthogonal"] 
  in
  let rec cast_list_of_vars l =
    match l with
      [] -> [] 
    | Var(s)::r -> Fol.Var(s)::(cast_list_of_vars r)
    | _ -> raise Not_found
  in
  let get_pred s =
    let lexbuf = Lexing.from_string s in
    let expr = Parser_expressions.start Lexer_expressions.expr lexbuf in 
    match expr with 
    | If(FunApp(s,args),_,_) | FunApp(s,args) ->
	(try
	  if List.mem s pred_list then
	    Some(Formulas.Atom(Fol.R(s,cast_list_of_vars args)))
	  else 
	    None
	with _ -> None)
    | _ -> None
  in
  let unpack o = match o with None -> assert false| Some a -> a in
  let l = extract_expressions s in 
  let process_command c = 
    match c with
      Meta s -> None
    | Expr s -> get_pred s
  in
  let preds = 
    List.map unpack 
      (List.filter (fun s -> s<> None) (List.map process_command l)) in
  let put_and x y = Formulas.And(x,y) in
  Prop.simplify (List.fold_left put_and  Formulas.True preds)  

(*
let essais = 
  eval_string "toto";
  eval_string "1.5";
  eval_string ""; 
  eval_string "##";
  eval_string "#0.5##0.6#"; 
  eval_string "a : #0.5# b : #0.6# voila";
  eval_string "toto#1.5#titi";
  eval_string "#.5#";
  eval_string "#1.#";
  eval_string "#1.5 + 1#";
  eval_string "#1+1#";
  eval_string "#2*2#";
  eval_string "#2-2#";
  eval_string "#2/2#";
  eval_string "#2+3*4#";
  eval_string "#-1#";
  eval_string "#- - 1#";
  eval_string "#-1+2*3-5-3#";
  eval_string "#let a = 1 in 1#";
  eval_string "#let a = 1 in a+a#";
  eval_string "#let a = 1+2 in a#";
  eval_string "#let a = 1 in let a=2 in a#";
  eval_string "#let a = 1 in let b=2 in a+b#";
  eval_string "#if true then 1 else 2#";
  eval_string "#if false then 1 else 2#";
  eval_string "#true#";
  eval_string "#false#";
  eval_string "#true or false#";
  eval_string "#true and false#";
  eval_string "#false and true#";
  eval_string "#true and true#";
  eval_string "#let a = true in a#";
  eval_string "#let a = true in if not a then 1 else 2#";
  eval_string "#cos(2)#";
  eval_string "#sin(1)#";
  eval_string "#tan(1)#";
  eval_string "#abs(-2)#";
  eval_string "#abs(1)#";
  eval_string "#sqrt(4)#";
  eval_string "#min(1,2)#";
  eval_string "#max(1,2)#";
  eval_string "#arccos(1)#";
  eval_string "#arcsin(1)#";
  eval_string "#arctan(1)#";
  eval_string "#1 < 2#";
  eval_string "#1 < 0#";
  eval_string "#1 > 2#";
  eval_string "#1 > 0#";
  eval_string "#1 <= 2#";
  eval_string "#1 <= 0#";
  eval_string "#1 >= 2#";
  eval_string "#1 >= 0#";
  eval_string "#1 = 2#";
  eval_string "#1 = 1#"; 
  eval_string "#1 <> 1#";
  eval_string "#1 <> 2#";
  eval_string "#exp(2)#";
  eval_string "#exp(1)#";
  eval_string "#pow(2,2)#";
  eval_string "#ln(2)#";
  eval_string "#log(10,2)#";
  eval_string "#e + 1#";
  eval_string "#pi#";
  eval_string "#area(a,b,c)#";  
  eval_string "#\"toto\"#";  
  eval_string "#if 1>2 then \"toto\" else \"titi\"#";  

*) 

  
