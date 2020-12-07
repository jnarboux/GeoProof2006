
(* $Id *)

open Types_de_base
open Objets_graphiques
open Objets_point_clique
open Objets_texte_clique 
open Objets_texte_point
open Objets_point_milieu
open Objets_point_sur_droite
open Objets_point_sur_cercle
open Objets_intersection_droites
open Objets_intersection_cercle_droite
open Objets_intersection_cercles
open Objets_droite_deux_points
open Objets_demi_droite
open Objects_point_left_turn
open Objects_point_between
open Objets_segment_deux_points
open Objets_vecteur_deux_points
open Objets_droite_orthogonale
open Objets_droite_mediatrice
open Objets_droite_parallele
open Objets_droite_bissectrice
open Objets_cercle_centre_point
open Objets_cercle_trois_points
open Objets_cercle_diametre
open Objets_point_transforme
open Objets_droite_transforme
open Objets_cercle_transforme
open Objects_segment_marks
open Objets_test_congruent_segments
open Couleurs 
open Fonctions
open Interface 
open I18n
open Options
open Optionsdeconfiguration
open Createur_de_noms
open Couche
open Geometric_functions.GeometryFunctions
open Document_gui

type state =
  | NONE
  | TEXTE_CLIQUE
  | TEXTE_POINT
  | POINT_CLIQUE
  | POINT_MILIEU_1
  | POINT_MILIEU_2 of t_objet_graphique
  | POINT_SUR_DROITE
  | POINT_SUR_CERCLE
  | INTERSECTION_DROITE_DROITE_1
  | INTERSECTION_DROITE_DROITE_2 of t_objet_graphique
  | INTERSECTION_CERCLE_DROITE_1
  | INTERSECTION_CERCLE_DROITE_2 of t_objet_graphique
  | INTERSECTION_CERCLE_CERCLE_1
  | INTERSECTION_CERCLE_CERCLE_2 of t_objet_graphique
  | POINT_LEFT_TURN_1
  | POINT_LEFT_TURN_2 of t_objet_graphique
  | POINT_LEFT_TURN_3 of t_objet_graphique * t_objet_graphique
  | POINT_BETWEEN_1
  | DROITE_DEUX_POINT_1
  | DROITE_DEUX_POINT_2 of t_objet_graphique
  | DEMIE_DROITE_DEUX_POINT_1
  | DEMIE_DROITE_DEUX_POINT_2 of t_objet_graphique
  | MEDIATRICE_1
  | MEDIATRICE_2 of t_objet_graphique
  | SEGMENT_1
  | SEGMENT_2 of t_objet_graphique
  | VECTEUR_1
  | VECTEUR_2 of t_objet_graphique
  | DROITE_PARALLELE_1
  | DROITE_PARALLELE_2 of t_objet_graphique
  | DROITE_ORTHOGONALE_1
  | DROITE_ORTHOGONALE_2 of t_objet_graphique
  | BISSECTRICE_ANGLE_1 
  | BISSECTRICE_ANGLE_2 of t_objet_graphique
  | BISSECTRICE_ANGLE_3 of t_objet_graphique * t_objet_graphique
  | CERCLE_CENTRE_POINT_1
  | CERCLE_CENTRE_POINT_2 of t_objet_graphique
  | CERCLE_TROIS_POINTS_1
  | CERCLE_TROIS_POINTS_2 of t_objet_graphique
  | CERCLE_TROIS_POINTS_3 of t_objet_graphique * t_objet_graphique
  | CERCLE_DIAMETRE_1
  | CERCLE_DIAMETRE_2 of t_objet_graphique
  | TRANSLATION_1
  | TRANSLATION_2 of t_objet_graphique
  | SYMETRIE_POINT_1
  | SYMETRIE_POINT_2 of t_objet_graphique
  | SYMETRIE_DROITE_1
  | SYMETRIE_DROITE_2 of t_objet_graphique
  | TEST_ALIGNEMENT_1 
  | TEST_ALIGNEMENT_2 of t_objet_graphique
  | TEST_ALIGNEMENT_3 of t_objet_graphique * t_objet_graphique
  | TEST_POINTS_CONFONDUS_1
  | TEST_POINTS_CONFONDUS_2  of t_objet_graphique
  | TEST_PARALLELISME_1
  | TEST_PARALLELISME_2  of t_objet_graphique
  | TEST_PERPENDICULARITE_1
  | TEST_PERPENDICULARITE_2  of t_objet_graphique
  | TEST_CONGRUENT_SEGMENTS_1 
  | TEST_CONGRUENT_SEGMENTS_2 of t_objet_graphique
  | TEST_CONGRUENT_ANGLES_1
  | TEST_CONGRUENT_ANGLES_2 of t_objet_graphique
  | TEST_CONGRUENT_ANGLES_3 of t_objet_graphique * t_objet_graphique
  | TEST_CONGRUENT_ANGLES_4 of t_objet_graphique * t_objet_graphique * t_objet_graphique
  | TEST_CONGRUENT_ANGLES_5 of t_objet_graphique * t_objet_graphique * t_objet_graphique * t_objet_graphique
  | TEST_CONGRUENT_ANGLES_6 of t_objet_graphique * t_objet_graphique * t_objet_graphique * t_objet_graphique * t_objet_graphique 
  | TEST_LEFT_TURN_1
  | TEST_LEFT_TURN_2 of t_objet_graphique
  | TEST_LEFT_TURN_3 of t_objet_graphique * t_objet_graphique
  | TEST_BETWEEN_1
  | TEST_BETWEEN_2 of t_objet_graphique
  | TEST_BETWEEN_3 of t_objet_graphique * t_objet_graphique
  | MESURE_ANGLE_1 
  | MESURE_ANGLE_2 of t_objet_graphique
  | MESURE_ANGLE_3 of t_objet_graphique * t_objet_graphique
  | MESURE_DISTANCE_1 
  | MESURE_DISTANCE_2 of t_objet_graphique
  | MESURE_AREA_1
  | MESURE_AREA_2 of t_objet_graphique
  | MESURE_AREA_3 of t_objet_graphique * t_objet_graphique
  | BOUGE_ATTEND
  | BOUGE_BOUGE of t_objet_graphique
  | GOMME_OBJET
  | SELECTION_OBJET
  | MOVE_PAPER 
  | MOVE_PAPER_2 of t_coords

(* Initialise Gtk afin de pouvoir créer l'objet fenêtre *)

let _ = GtkMain.Main.init()
let etat = ref NONE
let last_create_state = ref NONE
let fenetre = new fenetre_principale ()  


let update_pix() =
  (* on efface l'écran *)
  (*
    fenetre#drawingbuffer#set_foreground !!couleur_fond;
    fenetre#drawingbuffer#rectangle ~x:0 ~y:0
    ~width:!largeur_ecran ~height:!hauteur_ecran ~filled:true ();
  *)
  fenetre#document_gui.drawingbuffer#put_pixmap ~x:0 ~y:0 fenetre#document_gui.drawingbackbuffer#pixmap; 
  
  (* puis on demande à la liste de se redessiner dans le buffer *)
  
  let rep=Repere.get() in
  let dessine_un_objet x =  
    if (x#objet_visible) 
    then x#dessiner fenetre#document_gui.drawingbuffer fenetre#document_gui.drawingbackbuffer rep in
    Construction.iter dessine_un_objet; 
    fenetre#document_gui.drawing#put_pixmap ~x:0 ~y:0 fenetre#document_gui.drawingbuffer#pixmap    
      
let update_svg () =
  let pix = Rsvg.render_from_string (Output_svg.to_string ()) in
  fenetre#document_gui.svg_area#set_pixbuf pix 
    
let update_natural_language () =  
  let buffer = fenetre#document_gui.text_area#buffer in
  let start = buffer#get_iter `START in
  let stop = buffer#get_iter `END in
    buffer#delete ~start:start ~stop:stop;
    buffer#insert ~iter:start  ~tag_names:["not_editable"] (utf8 (Output_natural_language.to_string ()))  
      

let redraw_tab n =
  match n with
      0 -> update_pix()
    | 1 -> update_natural_language();
    | 2 -> update_svg()
    | _ -> assert false
	
let redessine_figure () =
  redraw_tab (fenetre#document_gui.notebook#current_page)
    
    
let print_message s = 
  let s = i18n s in
  let message_context = fenetre#statusbar#new_context s  in
  let _ = message_context#pop in
    ignore(message_context#push s)
      (* Le retour de cette fonction est utile que si l'on 
	 veut effacer un message qui n'est pas au sommet de la pile *)
      
let print_message_trop_loin () = print_message "Too far !"
  
let defaire_derniere_action () = 
  try 
    (* MG defaire_construction(); *)
    Document_managment.undo();
    redessine_figure ()
  with 
      Document_managment.CanNotCancel -> print_message "Undo stack empty"
	
let refaire_derniere_action () =
  try 
    Document_managment.redo();
    redessine_figure ()
  with 
      Document_managment.CanNotCancel -> print_message "Redo stack empty"
	
	

let delete_traces () =
  fenetre#document_gui.drawingbackbuffer#set_foreground !!couleur_fond;
  fenetre#document_gui.drawingbackbuffer#rectangle ~x:0 ~y:0
    ~width:(to_int !Repere.screen_width) ~height:(to_int !Repere.screen_height) ~filled:true ();
  redessine_figure ()

(* Essayer de choisir un objet dont le type est dans une liste 
   et appliquer f en cas de succes *)
	
let choisis_objet_gen obj_proches gev typeliste expt f =
  let ev = 
    {x = of_float (GdkEvent.Button.x gev); 
     y = of_float (GdkEvent.Button.y gev)}
  in
  let l = obj_proches (Repere.get()) ev typeliste expt in
    match l with
      | [] -> print_message_trop_loin ()
      | [o] -> f o
      | _ ->
	  (* Debug.pdb "Plusieurs possibilites"; *)
	  (* Il y a plusieurs objets qui conviennent donc 
	     on crée un menu pour permettre de choisir *)
	  let listeentree =
	    List.map
	      (fun o -> `I (o#objet_nom, (fun () ->  
		(*			    Debug.pdb "On a choisi :"; 
					    Debug.pdb (o#objet_nom); *)
					    f o)))
	      l
	  in
	    GToolbox.popup_menu
	      ~entries:listeentree
	      ~button:(GdkEvent.Button.button gev)
	      ~time:(GdkEvent.Button.time gev)
	      
	      
let text t = match t with
    TYPE_POINT -> "point"
  | TYPE_DROITE -> "droite"
  | TYPE_SEGMENT -> "segment"
  | TYPE_CERCLE -> "cercle"
  | TYPE_VECTEUR -> "vecteur"
  | TYPE_REPERE -> "repere"
  | TYPE_TEXTE -> "texte"
  | TYPE_MARK -> "mark"

(* The interresting objects are all object in the type list except the list of exceptions *)

let interresting_objects state = match state with
  | NONE | TEXTE_CLIQUE | POINT_CLIQUE -> [],[]
  | TEXTE_POINT -> [TYPE_POINT],[]
  | POINT_MILIEU_1 -> [TYPE_POINT],[]
  | POINT_MILIEU_2 x -> [TYPE_POINT],[x]
  | POINT_SUR_DROITE ->  [TYPE_DROITE],[]
  | POINT_SUR_CERCLE ->  [TYPE_CERCLE],[]
  | INTERSECTION_DROITE_DROITE_1 ->  [TYPE_DROITE],[]
  | INTERSECTION_DROITE_DROITE_2 x ->  [TYPE_DROITE],[x]
  | INTERSECTION_CERCLE_DROITE_1 ->  [TYPE_CERCLE],[]
  | INTERSECTION_CERCLE_DROITE_2 x ->  [TYPE_DROITE],[x]
  | INTERSECTION_CERCLE_CERCLE_1 ->  [TYPE_CERCLE],[]
  | INTERSECTION_CERCLE_CERCLE_2 x ->  [TYPE_CERCLE],[x]
  | POINT_LEFT_TURN_1 -> [TYPE_POINT],[]
  | POINT_LEFT_TURN_2 x -> [TYPE_POINT],[x]
  | POINT_LEFT_TURN_3(_,_) -> [],[]
  | POINT_BETWEEN_1 -> [TYPE_SEGMENT],[]
  | DROITE_DEUX_POINT_1  -> [TYPE_POINT],[]
  | DROITE_DEUX_POINT_2 x -> [TYPE_POINT],[x]
  | DEMIE_DROITE_DEUX_POINT_1  -> [TYPE_POINT],[]
  | DEMIE_DROITE_DEUX_POINT_2 x -> [TYPE_POINT],[x]
  | MEDIATRICE_1 -> [TYPE_POINT],[]
  | MEDIATRICE_2 x -> [TYPE_POINT],[x]
  | SEGMENT_1 -> [TYPE_POINT],[]
  | SEGMENT_2 x -> [TYPE_POINT],[x]
  | VECTEUR_1 -> [TYPE_POINT],[]
  | VECTEUR_2 x -> [TYPE_POINT],[x]
  | DROITE_PARALLELE_1 ->  [TYPE_DROITE;TYPE_SEGMENT],[]
  | DROITE_PARALLELE_2 _ -> [TYPE_POINT],[]
  | DROITE_ORTHOGONALE_1 ->  [TYPE_DROITE;TYPE_SEGMENT],[]
  | DROITE_ORTHOGONALE_2 _ -> [TYPE_POINT],[]
  | BISSECTRICE_ANGLE_1  -> [TYPE_POINT],[]
  | BISSECTRICE_ANGLE_2 x -> [TYPE_POINT],[x]
  | BISSECTRICE_ANGLE_3(x,y) -> [TYPE_POINT],[x;y]
  | CERCLE_CENTRE_POINT_1 -> [TYPE_POINT],[]
  | CERCLE_CENTRE_POINT_2 x -> [TYPE_POINT],[x]
  | CERCLE_TROIS_POINTS_1 -> [TYPE_POINT],[]
  | CERCLE_TROIS_POINTS_2 x -> [TYPE_POINT],[x]
  | CERCLE_TROIS_POINTS_3(x,y) -> [TYPE_POINT],[x;y]
  | CERCLE_DIAMETRE_1 -> [TYPE_POINT],[]
  | CERCLE_DIAMETRE_2 x -> [TYPE_POINT],[x]
  | TRANSLATION_1 -> [TYPE_VECTEUR],[]
  | TRANSLATION_2 _ ->  [TYPE_POINT;TYPE_DROITE;TYPE_CERCLE],[]
  | SYMETRIE_POINT_1 -> [TYPE_POINT],[]
  | SYMETRIE_POINT_2 x ->  [TYPE_POINT;TYPE_DROITE;TYPE_CERCLE],[x]
  | SYMETRIE_DROITE_1 ->  [TYPE_DROITE],[]
  | SYMETRIE_DROITE_2 x ->  [TYPE_POINT;TYPE_DROITE;TYPE_CERCLE],[x]
  | TEST_ALIGNEMENT_1  -> [TYPE_POINT],[]
  | TEST_ALIGNEMENT_2 x -> [TYPE_POINT],[x]
  | TEST_ALIGNEMENT_3(x,y) -> [TYPE_POINT],[x;y]
  | TEST_POINTS_CONFONDUS_1 -> [TYPE_POINT],[]
  | TEST_POINTS_CONFONDUS_2  x -> [TYPE_POINT],[x]
  | TEST_PARALLELISME_1 ->  [TYPE_DROITE],[]
  | TEST_PARALLELISME_2  x ->  [TYPE_DROITE],[x]
  | TEST_PERPENDICULARITE_1 ->  [TYPE_DROITE],[]
  | TEST_PERPENDICULARITE_2  x ->  [TYPE_DROITE],[x]
  | TEST_CONGRUENT_SEGMENTS_1  ->  [TYPE_SEGMENT],[]
  | TEST_CONGRUENT_SEGMENTS_2 x ->  [TYPE_SEGMENT],[x]
  | TEST_CONGRUENT_ANGLES_1
  | TEST_CONGRUENT_ANGLES_2 _
  | TEST_CONGRUENT_ANGLES_3(_,_) 
  | TEST_CONGRUENT_ANGLES_4(_,_,_) 
  | TEST_CONGRUENT_ANGLES_5(_,_,_,_)
  | TEST_CONGRUENT_ANGLES_6(_,_,_,_,_) -> [TYPE_POINT],[]
  | TEST_LEFT_TURN_1 -> [TYPE_POINT],[]
  | TEST_LEFT_TURN_2 x -> [TYPE_POINT],[x]
  | TEST_LEFT_TURN_3(x,y) -> [TYPE_POINT],[x;y]
  | TEST_BETWEEN_1 -> [TYPE_POINT],[]
  | TEST_BETWEEN_2 x -> [TYPE_POINT],[x]
  | TEST_BETWEEN_3(x,y) -> [TYPE_POINT],[x;y]
  | MESURE_ANGLE_1 | MESURE_ANGLE_2 _ | MESURE_ANGLE_3(_,_) 
  | MESURE_DISTANCE_1 | MESURE_DISTANCE_2 _ 
  | MESURE_AREA_1 | MESURE_AREA_2 _ | MESURE_AREA_3(_,_) -> [TYPE_POINT],[]
  | BOUGE_ATTEND -> [TYPE_POINT;TYPE_TEXTE],[]
  | BOUGE_BOUGE _ -> [],[]
  | GOMME_OBJET | SELECTION_OBJET -> tout_type_objet,[]
  | MOVE_PAPER | MOVE_PAPER_2 _ -> [],[]


let choisis_objet gev f = 
  let typeliste,expt = interresting_objects !etat in
    choisis_objet_gen Construction.objets_proches gev typeliste expt f
    
let choisis_objet_libre gev f =
  let typeliste,expt = interresting_objects !etat in
  choisis_objet_gen Construction.objets_libres_proches gev typeliste expt f
    
let rec initialise_objet o = 
  o#change_couleur !!couleur_defaut;
  o#change_epaisseur !!epaisseur_defaut;
  begin
    match 
      o#objet_type 
    with
	TYPE_DROITE ->  o#nomme (nom_nouveau_droite())
      | TYPE_SEGMENT -> o#nomme (nom_nouveau_segment())
      | TYPE_CERCLE -> o#nomme (nom_nouveau_cercle())
      | TYPE_VECTEUR -> o#nomme (nom_nouveau_vecteur())
      | TYPE_REPERE -> o#nomme (nom_nouveau_repere())
      | TYPE_POINT -> 
	  begin
	    o#nomme (nom_nouveau_point());
	    o#change_couleur !!couleur_point_defaut
	  end
      | TYPE_TEXTE -> o#nomme (nom_nouveau_texte())
      | TYPE_MARK -> o#nomme (name_new_mark())
  end;
  begin
    match 
      o#objet_type 
    with
	TYPE_DROITE | TYPE_SEGMENT | TYPE_CERCLE | TYPE_VECTEUR ->
	  o#change_style (STYLE_DROITE !!style_droite_defaut)
      | TYPE_POINT ->
	  o#change_style (STYLE_POINT !!style_point_defaut)
      | TYPE_TEXTE -> o#recalcule
      | TYPE_REPERE -> 
	  o#change_style (STYLE_REPERE {grid_shown=true})
      | TYPE_MARK ->
	  o#change_style (STYLE_MARK (LINE 1))
  end;
  Document_managment.add_object o;
  Coq_proofs.output_coq_for_creation o;
  if
    !!automatic_point_label && o#objet_type = TYPE_POINT 
  then
    initialise_objet (new objet_texte_point o o#objet_nom)
 
let supprimme_objet x =
  List.iter (fun o -> Coq_proofs.output_coq_for_delete o ()) 
    (Document_managment.depending_objects x);
  Document_managment.remove_object x;
  print_message "Object deleted";		       
  redessine_figure ()
    
let on_state_change st message x =
  x#highlight_object CONSTRUCTION;
  redessine_figure();
  etat := (st x);
  print_message message

let bouton1_click gev  =
  begin
    let ev = 
      let x = GdkEvent.Button.x gev
      and y = GdkEvent.Button.y gev in
	Repere.ecran_vers_papier {x = of_float x; y = of_float y} ()
    in
    let choose_object = choisis_objet gev in
      match !etat with
	  
	| TEXTE_CLIQUE -> 
	    let p = (new objet_texte_clique ev "label") in
	      initialise_objet p;
	      redessine_figure () 
		
	| TEXTE_POINT ->
	    choose_object
	      (fun x ->
		 initialise_objet (new objet_texte_point x x#objet_nom);
		 print_message "point label created.";
		 redessine_figure ())
	      
	| POINT_CLIQUE ->
	    let p = (new objet_point_clique ev) in
	      initialise_objet p;
	      redessine_figure () 
		
	| POINT_MILIEU_1 ->
	    choose_object
	      (on_state_change 
	      (fun x -> POINT_MILIEU_2 x) 
	      "Point acquired, choose the second point.")
	| POINT_MILIEU_2 point ->
	    choose_object
	      (fun x ->
		 initialise_objet
		   (new objet_point_milieu point x);
		 etat := POINT_MILIEU_1; 
		 point#unhighlight_object CONSTRUCTION;
		 print_message "Midpoint created.";
		 redessine_figure ())
	      
	| POINT_SUR_DROITE ->
	    choose_object
	      (fun x ->
		 initialise_objet
		   (new objet_point_sur_droite x  ev);
		 print_message "Line acquired. Point created.";
		 redessine_figure ())
	      
	| POINT_SUR_CERCLE ->
	    choose_object
	      (fun x ->
		 initialise_objet
		   (new objet_point_sur_cercle x ev);
		 print_message "Circle acquired. Point created.";
		 redessine_figure ())
	      
	| POINT_BETWEEN_1 ->
	    choose_object
	      (fun segment ->
		 initialise_objet
		   (new object_point_between ev segment);
		 print_message "Point on segment created.";
		 redessine_figure ()
	      )
	      
	| INTERSECTION_DROITE_DROITE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := INTERSECTION_DROITE_DROITE_2 x;
		 print_message "Line acquired, choose a line.";
	      )
	      
	| INTERSECTION_DROITE_DROITE_2 droite ->
	    choose_object
	      (fun x ->
		 initialise_objet
		   (new objet_intersection_droite_droite droite x);
		 droite#unhighlight_object CONSTRUCTION;
		 etat := INTERSECTION_DROITE_DROITE_1;
		 print_message "Point created.";
		 redessine_figure ())
	      
	| INTERSECTION_CERCLE_DROITE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := INTERSECTION_CERCLE_DROITE_2 x;
		 print_message "Circle acquired, choose a line.";
	      )
	      
	| INTERSECTION_CERCLE_DROITE_2 cercle ->
	    choose_object
	      (fun x ->
		 initialise_objet
		   (new objet_intersection_cercle_droite cercle x 1);
		 initialise_objet
		   (new objet_intersection_cercle_droite cercle x (-1));
		 cercle#unhighlight_object CONSTRUCTION;
		 etat := INTERSECTION_CERCLE_DROITE_1;
		 print_message "Point(s) created(s)";
		 redessine_figure ())
	      
	      
	| INTERSECTION_CERCLE_CERCLE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := INTERSECTION_CERCLE_CERCLE_2 x;
		 print_message "Circle acquis, choose a line.";
	      )
	      
	| INTERSECTION_CERCLE_CERCLE_2 cercle ->
	    choose_object
	      (fun x ->
		 initialise_objet
		   (new objet_intersection_cercles cercle x 1);
		 initialise_objet
		   (new objet_intersection_cercles cercle x (-1));
		 cercle#unhighlight_object CONSTRUCTION;
		 etat := INTERSECTION_CERCLE_CERCLE_1;
		 print_message "Points created.";
		 redessine_figure ())
	      
	| POINT_LEFT_TURN_1 ->
	    choose_object
	      (fun premier_point ->
		 premier_point#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := POINT_LEFT_TURN_2 premier_point;
		 print_message "Point acquired, choose the second point."
	      )
	      
	| POINT_LEFT_TURN_2 point1 ->
	    choose_object
	      (fun point2 ->
		 point2#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := POINT_LEFT_TURN_3 (point1,point2);
		 print_message "Point acquired, choose the initial position."
	      )
	  
    | POINT_LEFT_TURN_3 (point1,point2) ->
	initialise_objet 
	  (new object_point_left_turn ev point1 point2);
	etat := POINT_LEFT_TURN_1;
	point1#unhighlight_object CONSTRUCTION;
	point2#unhighlight_object CONSTRUCTION;
	print_message "Left turn created.";
	redessine_figure ()
	  
    | DROITE_DEUX_POINT_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := DROITE_DEUX_POINT_2 x;
		 print_message "Point acquired, choose another point.";
	      )

    | DROITE_DEUX_POINT_2 point ->
	    choose_object
	      (fun x ->
		initialise_objet
		  (new objet_droite_deux_points point x);
		point#unhighlight_object CONSTRUCTION;
		etat := DROITE_DEUX_POINT_1;
		print_message "Line created.";
		redessine_figure ())

    | DEMIE_DROITE_DEUX_POINT_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := DEMIE_DROITE_DEUX_POINT_2 x;
		 print_message "Point acquired, choose another point.";
	      )

    | DEMIE_DROITE_DEUX_POINT_2 point ->
	    choose_object
	      (fun x -> (); 
	(*	 initialise_objet
		   (new objet_demi_droite point x); *)
		 point#unhighlight_object CONSTRUCTION;
		 etat := DEMIE_DROITE_DEUX_POINT_1;
		 print_message "Half-Line created.";
		 redessine_figure ())

    | MEDIATRICE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := MEDIATRICE_2 x;
		 print_message "Point acquired, choose the second point.";
	      )

    | MEDIATRICE_2 point ->
	    choose_object
	      (fun x ->
		initialise_objet
		  (new objet_droite_mediatrice point x);
		etat := MEDIATRICE_1;
		point#unhighlight_object CONSTRUCTION;
		print_message "Perpendicular bissector created.";
		redessine_figure ())

    | SEGMENT_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := SEGMENT_2 x;
		 print_message "Point acquired, choose a second point.";
	      )

    | SEGMENT_2 point ->
	    choose_object
	      (fun x ->
		 let s = (new objet_segment_deux_points point x) in
		   initialise_objet s;
		   point#unhighlight_object CONSTRUCTION;
		   etat := SEGMENT_1;
		   print_message "Segment created.";       
		   redessine_figure ())

    | VECTEUR_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := VECTEUR_2 x;
		 print_message "Point acquired, choose a second point.";
	      )

    | VECTEUR_2 point ->
	    choose_object
	      (fun x ->
		initialise_objet
		  (new objet_vecteur_deux_points point x);
		etat := VECTEUR_1;
		point#unhighlight_object CONSTRUCTION;
		print_message "Vector created.";
		redessine_figure ())

    | DROITE_PARALLELE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := DROITE_PARALLELE_2 x;
		 print_message "Line acquired, choose a point.";
	      )

    | DROITE_PARALLELE_2 point ->
	    choose_object
	      (fun x ->
		initialise_objet
		  (new objet_droite_parallele x point);
		etat := DROITE_PARALLELE_1;
		point#unhighlight_object CONSTRUCTION;
		print_message "Line created.";
		redessine_figure ())

    | DROITE_ORTHOGONALE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := DROITE_ORTHOGONALE_2 x;
		 print_message "Line acquired, choose a point.";
	      )

    | DROITE_ORTHOGONALE_2 droite ->
	    choose_object
	      (fun x ->
		initialise_objet
		  (new objet_droite_orthogonale x droite);
		etat := DROITE_ORTHOGONALE_1;
		droite#unhighlight_object CONSTRUCTION;
		print_message "Line created.";
		redessine_figure ())

    | CERCLE_CENTRE_POINT_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := CERCLE_CENTRE_POINT_2 x;
		 print_message "Centre acquired, choose a point.";
	      )

    | CERCLE_CENTRE_POINT_2 point ->
	    choose_object
	      (fun x ->
		initialise_objet
		  (new objet_cercle_centre_point point x);
		etat := CERCLE_CENTRE_POINT_1;
		point#unhighlight_object CONSTRUCTION;
		print_message "Circle created.";
		redessine_figure ())
	     
    | CERCLE_TROIS_POINTS_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := CERCLE_TROIS_POINTS_2 x;
		 print_message "First point acquired, choose another point.";
	      )
   
    | CERCLE_TROIS_POINTS_2 point1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		etat := CERCLE_TROIS_POINTS_3 (point1,x);
		print_message "Second point acquired, choose the third point.";
	      )

    | CERCLE_TROIS_POINTS_3 (point1,point2) ->
	    choose_object
	      (fun x ->
		initialise_objet
		  (new objet_cercle_trois_points point1 point2 x);
		etat := CERCLE_TROIS_POINTS_1;
		point1#unhighlight_object CONSTRUCTION;
		point2#unhighlight_object CONSTRUCTION;
		print_message "Circle created.";
		redessine_figure ())

    | CERCLE_DIAMETRE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := CERCLE_DIAMETRE_2 x;
		 print_message
		   "Point acquired, choose another point."
	      )

    | CERCLE_DIAMETRE_2 point ->
	    choose_object
	      (fun x ->	
		initialise_objet
		  (new objet_cercle_diametre point x);
		etat := CERCLE_DIAMETRE_1;
		point#unhighlight_object CONSTRUCTION;
		print_message "Circle created.";	
		redessine_figure ())
	     
    | TRANSLATION_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure();
		 etat := TRANSLATION_2 x;
		 print_message
		   "Vector acquired, choose the object you want to translate."
	      )

    | TRANSLATION_2 vec ->
	choose_object
	  (fun x ->
	     vec#unhighlight_object CONSTRUCTION;
	     match x#objet_type with
	       | TYPE_POINT ->	
		   initialise_objet
		     (new objet_point_transforme x vec);
		   etat := TRANSLATION_1;
		   print_message "Point translated.";
		   redessine_figure ()
	       | TYPE_DROITE -> 
		   initialise_objet
		     (new objet_droite_transforme x vec);
		   etat := TRANSLATION_1;
		   print_message "Line translated";
		   redessine_figure ()
	       | TYPE_CERCLE ->
		   initialise_objet
		     (new objet_cercle_transforme x vec);
		   etat := TRANSLATION_1;
		   print_message "Circle translated";
		   redessine_figure ()
	       | TYPE_SEGMENT ->
                   (*
		     initialise_objet
		     (new objet_segment_transforme x vec); *)
		   etat := TRANSLATION_1;
		   print_message "Segment translated";
		   redessine_figure ()
		     
	       | _ -> assert false
	  )	   
	  
    | SYMETRIE_POINT_1 ->
	choose_object
	  (fun x ->
	     x#highlight_object CONSTRUCTION;
	     redessine_figure(); 
	     etat := SYMETRIE_POINT_2 x;
	     print_message
		  "Center acquired ; choose the object you want to translate."
	  )
	  
    | SYMETRIE_POINT_2 centre ->
	choose_object
	  (fun x ->
	     centre#unhighlight_object CONSTRUCTION;
	     match x#objet_type with
	       | TYPE_POINT ->
		   initialise_objet
		     (new objet_point_transforme x centre);
		   etat := SYMETRIE_POINT_1;
		   print_message "Symetrical point created";
		   redessine_figure ()
	       | TYPE_DROITE ->
		   initialise_objet
		     (new objet_droite_transforme x centre);
		   etat := SYMETRIE_POINT_1;
		   print_message "Symetrical line created";		    
		   redessine_figure ()
	       | TYPE_CERCLE ->
		   initialise_objet
		     (new objet_cercle_transforme x centre);
		   etat := SYMETRIE_POINT_1;
		   print_message "Symetrical circle created";
		   redessine_figure ()
	       | TYPE_SEGMENT ->
		   (*
		     initialise_objet
		     (new objet_segment_transforme x centre); 
		   *)
		   etat := SYMETRIE_POINT_1;
		   print_message "Symetrical segment created";
		   redessine_figure ()
	       | _ -> assert false
	  )
	  
	  
    | SYMETRIE_DROITE_1 ->
	choose_object
	  (fun x ->
	     x#highlight_object CONSTRUCTION;
	     redessine_figure(); 
	     etat := SYMETRIE_DROITE_2 x;
	     print_message
	       "Line acquired, choose the object you want to translate."
	  )
	     
    | SYMETRIE_DROITE_2 droite ->
	choose_object
	  (fun x ->
	     droite#unhighlight_object CONSTRUCTION;
	     match x#objet_type with
	       | TYPE_POINT ->
		   initialise_objet
		     (new objet_point_transforme x droite);
		   etat := SYMETRIE_DROITE_1;
		   print_message "Symetrical point created.";
		   redessine_figure ()
	       | TYPE_DROITE ->
		   initialise_objet
		     (new objet_droite_transforme x droite);
		   etat := SYMETRIE_DROITE_1;
		   print_message "Symetrical line created.";
		   redessine_figure ()
	       | TYPE_CERCLE ->
		   initialise_objet
		     (new objet_cercle_transforme x droite);
		   etat := SYMETRIE_DROITE_1;
		   print_message "Symetrical circle created.";
		   redessine_figure ()
	       | TYPE_SEGMENT ->
		   (*
		     initialise_objet
		     (new objet_segment_transforme x droite);
		   *)
		   etat := SYMETRIE_DROITE_1;
		   print_message "Symetrical segment created.";
		   redessine_figure ()
	       | _ -> assert false
	  )
	  
    | TEST_ALIGNEMENT_1 ->
	choose_object
	  (fun x ->
	     x#highlight_object CONSTRUCTION;
	     redessine_figure(); 
	     etat := TEST_ALIGNEMENT_2 x;
	     print_message
	       "Point acquired, choose a second point."
	  )
	  
    | TEST_ALIGNEMENT_2 point ->
	    choose_object
	      (fun x ->	
		 x#highlight_object CONSTRUCTION;
		 redessine_figure(); 
		 etat := TEST_ALIGNEMENT_3 (point,x);
		 print_message
		  "Point acquired, choose a third point."
	      )

   | TEST_ALIGNEMENT_3 (point1,point2) ->
	    choose_object
	      (fun point3 ->
		 let p1n = point1#objet_nom in
		 let p2n = point2#objet_nom in
		 let p3n = point3#objet_nom in
		 let text = (p1n^","^p2n^" and "^p3n^" seem to be #if collinear("^p1n^","^p2n^","^p3n^") then \"\" else \"not \"#collinear") in
		   initialise_objet (new objet_texte_clique ev text);
		   let gui = 
		     fenetre#document_gui in
		     Studied_facts.add_fact gui.studied_facts "H" "tata" "darkgreen" ();
		     etat := TEST_ALIGNEMENT_1;
		     point1#unhighlight_object CONSTRUCTION;
		     point2#unhighlight_object CONSTRUCTION;
		     print_message "Collinearity test created.";	
		     redessine_figure ()
	      )

   | TEST_LEFT_TURN_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure(); 
		 etat := TEST_LEFT_TURN_2 x;
		 print_message
		  "Point acquired, choose a second point."
	      )

    | TEST_LEFT_TURN_2 point ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure(); 
		 etat := TEST_LEFT_TURN_3 (point,x);
		 print_message
		   "Point acquired, choose a third point."
	      )

   | TEST_LEFT_TURN_3 (point1,point2) ->
	    choose_object
	      (fun point3 ->
		let p1n = point1#objet_nom in
		let p2n = point2#objet_nom in
		let p3n = point3#objet_nom in
		let text = (p3n^" seem to be #if left_turn("^p1n^","^p2n^","^p3n^") then \"\" else \"not \"#on the left of "^p1n^p2n) in
		let gui = fenetre#document_gui.studied_facts in
		Document_managment.add_object_facts (new Object_studied_fact.studied_fact text gui);
		initialise_objet (new objet_texte_clique ev text);
		etat := TEST_LEFT_TURN_1;
		point1#unhighlight_object CONSTRUCTION;
		point2#unhighlight_object CONSTRUCTION;
		print_message "Left turn test created.";	
		redessine_figure ()
	      )

   | TEST_BETWEEN_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure(); 
		 etat := TEST_BETWEEN_2 x;
		 print_message
		   "Point acquired, choose a second point."
	      )

    | TEST_BETWEEN_2 point ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure(); 
		 etat := TEST_BETWEEN_3 (point,x);
		 print_message
		   "Point acquired, choose a third point."
	      )

   | TEST_BETWEEN_3 (point1,point2) ->
	    choose_object
	      (fun point3 ->
		let p1n = point1#objet_nom in
		let p2n = point2#objet_nom in
		let p3n = point3#objet_nom in
		let text = (p2n^" seem to be #if between("^p1n^","^p2n^","^p3n^") then \"\" else \"not \"#between "^p1n^" and "^p3n) in
		initialise_objet (new objet_texte_clique ev text);		
		etat := TEST_BETWEEN_1;
		point1#unhighlight_object CONSTRUCTION;
		point2#unhighlight_object CONSTRUCTION;
		print_message "Between test created.";	
		redessine_figure ()
	      )

   | TEST_POINTS_CONFONDUS_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure(); 
		 etat := TEST_POINTS_CONFONDUS_2 x;
		 print_message
		  "Point acquired, choose another point."
	      )

   | TEST_POINTS_CONFONDUS_2 point1 ->
       choose_object 
	 (fun point2 ->
	   let p1n = point1#objet_nom in
	   let p2n = point2#objet_nom in
	   let text = (p1n^" and "^p2n^" seem to be #if eq_points("^p1n^","^p2n^") then \"\" else \"not \"#equals") in
	   let p = (new objet_texte_clique ev text) in
	   initialise_objet p;
	   etat := TEST_POINTS_CONFONDUS_1;
	   point1#unhighlight_object CONSTRUCTION;
	   print_message "Egality test created.";	
	   redessine_figure ()
	 )

   | TEST_PARALLELISME_1 ->
       choose_object
	 (fun x ->
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := TEST_PARALLELISME_2 x;
	    print_message
	      "Line acquired, choose another line."
	 )

   | TEST_PARALLELISME_2 droite1 ->
	    choose_object
	      (fun droite2 ->
		let d1n = droite1#objet_nom in
		let d2n = droite2#objet_nom in
		let text = (d1n^" and "^d2n^" seem to be #if parallel("^d1n^","^d2n^") then \"\" else \"not \"#parallel") in
		let p = (new objet_texte_clique ev text) in	
		initialise_objet p;
		etat := TEST_PARALLELISME_1;
		droite1#unhighlight_object CONSTRUCTION;
		print_message "Parallelism test created.";	
		redessine_figure ()
	      )
 
   | TEST_PERPENDICULARITE_1 ->
	    choose_object
	      (fun x ->
		 x#highlight_object CONSTRUCTION;
		 redessine_figure(); 
		etat := TEST_PERPENDICULARITE_2 x;
		print_message
		  "Line acquired, choose another line."
	      )

   | TEST_PERPENDICULARITE_2 droite1 ->
	    choose_object
	      (fun droite2 ->
	        let d1n = droite1#objet_nom in
		let d2n = droite2#objet_nom in
		let text = (d1n^" and "^d2n^" seem to be #if orthogonal("^d1n^","^d2n^") then \"\" else \"not \"#orthogonal") in
		let p = (new objet_texte_clique ev text) in
		initialise_objet p;
		etat := TEST_PERPENDICULARITE_1;
		droite1#unhighlight_object CONSTRUCTION;
		print_message "Orthognality test created.";	
		redessine_figure ()
	      )

	 
   | TEST_CONGRUENT_SEGMENTS_1 ->
       choose_object
       (fun x ->
	  x#highlight_object CONSTRUCTION;
	  redessine_figure(); 
	  etat := TEST_CONGRUENT_SEGMENTS_2 x;
	  print_message
	    "Segments acquired, choose the second one."
       )

   | TEST_CONGRUENT_SEGMENTS_2 segment1 ->
       choose_object
	 (fun segment2 ->
	    (*
           let s1n = segment1#objet_nom in
	   let s2n = segment2#objet_nom in
	   let text = (s1n^" and "^s2n^" are #if eq_lengths("^s1n^","^s2n^") then \"\" else \"not \"#congruent") in
	   let p = (new objet_texte_clique ev text) in
	   initialise_objet p;
	    *)
	    let p =  (new objet_test_congruent_segments segment1 segment2) in
	      initialise_objet p;
	   etat := TEST_CONGRUENT_SEGMENTS_1;
	   segment1#unhighlight_object CONSTRUCTION;
	   print_message "Congruence test created.";	
	   redessine_figure ()
	 )

   | TEST_CONGRUENT_ANGLES_1 ->
       choose_object
       (fun x ->
	  x#highlight_object CONSTRUCTION;
	  redessine_figure(); 
	  etat := TEST_CONGRUENT_ANGLES_2 x;
	  print_message
	    "Point acquired, choose the second one of the first angle."
       )

   | TEST_CONGRUENT_ANGLES_2 point1 ->
       choose_object
	 (fun point2 ->	  
	    point2#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := TEST_CONGRUENT_ANGLES_3 (point1,point2);
	    print_message
	      "Point acquired, choose the third point of the first angle."
	 )
 
   | TEST_CONGRUENT_ANGLES_3 (point1,point2) ->
       choose_object
	 (fun point3 -> 
	    point3#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := TEST_CONGRUENT_ANGLES_4 (point1,point2,point3);
	    print_message
	      "Point acquired, choose the first point of the second angle."
	 )
  | TEST_CONGRUENT_ANGLES_4 (point1,point2,point3) ->
       choose_object
	 (fun point4 -> 
	    point4#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := TEST_CONGRUENT_ANGLES_5 (point1,point2,point3,point4);
	    print_message
	      "Point acquired, choose the second point of the second angle."
	 )
 
  | TEST_CONGRUENT_ANGLES_5 (point1,point2,point3,point4) ->
       choose_object
	 (fun point5 ->
	    point5#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := TEST_CONGRUENT_ANGLES_6 (point1,point2,point3,point4,point5);
	    print_message
	      "Point acquired, choose the third point of the second angle."
	 )

   | TEST_CONGRUENT_ANGLES_6 (p1,p2,p3,p4,p5) ->
       choose_object
	 (fun p6 ->
	   let p1n = p1#objet_nom in
	   let p2n = p2#objet_nom in
	   let p3n = p3#objet_nom in
	   let p4n = p4#objet_nom in
	   let p5n = p5#objet_nom in
	   let p6n = p6#objet_nom in
	   let text = ("The angles"^p1n^p2n^p3n^" and "^p4n^p5n^p6n^" seem to be #if eq_angles("^p1n^","^p2n^","^p3n^","^p4n^","^p5n^","^p6n^") then \"\" else \"not \"#equals") in
	   let p = (new objet_texte_clique ev text) in
	   initialise_objet p;
	   p1#unhighlight_object CONSTRUCTION;
	   p2#unhighlight_object CONSTRUCTION;
	   p3#unhighlight_object CONSTRUCTION;
	   p4#unhighlight_object CONSTRUCTION;
	   p5#unhighlight_object CONSTRUCTION;
	   etat := TEST_CONGRUENT_ANGLES_1;
	   print_message "Congruence test created.";	
	   redessine_figure ()
	 )

   | MESURE_ANGLE_1 ->
       choose_object
	 (fun x ->
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := MESURE_ANGLE_2 x;
	    print_message
	      "Point acquired, choose a second point."
	 )

   | MESURE_ANGLE_2 point ->
       choose_object
	 (fun x ->
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := MESURE_ANGLE_3 (point,x);
	    print_message
	     "Point acquired, choose a third point."
	 )

   | MESURE_ANGLE_3 (point1,point2) ->
       choose_object
	 (fun point3 ->
	   let p1n = point1#objet_nom in
	   let p2n = point2#objet_nom in
	   let p3n = point3#objet_nom in
	   let text = ("#angle("^p1n^","^p2n^","^p3n^")#") in
	   let p = (new objet_texte_clique ev text) in
	   initialise_objet p;
	   etat := MESURE_ANGLE_1;
	   point1#unhighlight_object CONSTRUCTION;
	   point2#unhighlight_object CONSTRUCTION;
	   print_message "Angle measure created.";	
	   redessine_figure ())


   | MESURE_AREA_1 ->
       choose_object
	 (fun x ->
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := MESURE_AREA_2 x;
	    print_message
	     "Point acquired, choose a second point."
	 )

   | MESURE_AREA_2 point ->
       choose_object
	 (fun x ->  
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := MESURE_AREA_3 (point,x);
	    print_message
	     "Point acquired, choose a third point."
	 )

   | MESURE_AREA_3 (point1,point2) ->
       choose_object
	 (fun point3 ->
	   let p1n = point1#objet_nom in
	   let p2n = point2#objet_nom in
	   let p3n = point3#objet_nom in
	   let text = ("#area("^p1n^","^p2n^","^p3n^")#") in
	   let p = (new objet_texte_clique ev text) in
	   initialise_objet p;
	   etat := MESURE_AREA_1;
	   point1#unhighlight_object CONSTRUCTION;
	   point2#unhighlight_object CONSTRUCTION;
	   print_message "Area measure created.";	
	   redessine_figure ())

 
  | BISSECTRICE_ANGLE_1 ->
       choose_object
	 (fun x ->
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := BISSECTRICE_ANGLE_2 x;
	    print_message
	      "Point acquired, choose another point."
	 )

   | BISSECTRICE_ANGLE_2 point ->
       choose_object
	 (fun x ->
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := BISSECTRICE_ANGLE_3 (point,x);
	    print_message
	      "Point acquired, choose a third point."
	 )

   | BISSECTRICE_ANGLE_3 (point1,point2) ->
       choose_object
	 (fun point3 ->
	   let p = (new objet_droite_bissectrice point1 point2 point3) in
	   initialise_objet p;
	   etat := BISSECTRICE_ANGLE_1;
	   point1#unhighlight_object CONSTRUCTION;
	   point2#unhighlight_object CONSTRUCTION;
	   print_message "Angle bissector created.";	
	   redessine_figure ())


   | MESURE_DISTANCE_1 ->
       choose_object
	 (fun x ->
	    x#highlight_object CONSTRUCTION;
	    redessine_figure(); 
	    etat := MESURE_DISTANCE_2 x;
	    print_message
	      "Point acquired, choose a second pointt."
	 )

   | MESURE_DISTANCE_2 point1 ->
       choose_object
	 (fun point2 ->
	   let p1n = point1#objet_nom in
	   let p2n = point2#objet_nom in
	   let text = ("#length("^p1n^","^p2n^")#") in
	   let p = (new objet_texte_clique ev text) in
	   initialise_objet p;
	   etat := MESURE_DISTANCE_1;
	   point1#unhighlight_object CONSTRUCTION;
	   print_message "Distance measure created.";	
	   redessine_figure ())

    | BOUGE_ATTEND ->
	choisis_objet_libre gev
	  (fun x ->
	    etat := BOUGE_BOUGE x;
	    print_message
	      "Object acquired, you can move it !"
	  )

    | BOUGE_BOUGE point -> () 	

    | MOVE_PAPER ->
	let starting_point = 
	  {x = of_float (GdkEvent.Button.x gev); 
	   y = of_float (GdkEvent.Button.y gev)}
	in
	  etat := MOVE_PAPER_2 starting_point;
	  print_message "Move the pointer.";

    | MOVE_PAPER_2 _ -> ()

    | GOMME_OBJET ->
	choose_object supprimme_objet

    | SELECTION_OBJET -> 
	choose_object 
	  (fun x ->
	    if  x#objet_selectionne 
	    then x#deselectionne_objet
	    else x#selectionne_objet;
	    redessine_figure ()
	  )
    | NONE -> ()
  end


let change_state s = 
  (match s with
     | BOUGE_ATTEND | BOUGE_BOUGE _ -> ()
     | _ -> (Construction.unhighlight_objects MOVABLE ();redessine_figure()));
  etat := s


let aucunEtat () = 
  change_state NONE;
  print_message "State : None."
    
let texteLibre () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEXTE_CLIQUE;
  print_message "State : Free label."
    
let textePoint () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEXTE_POINT;
  print_message "State : Point label."
    
let pointLibre () =
  fenetre#utiliser_pointeur_creer ();
  change_state POINT_CLIQUE;
  print_message "State : Free point."

let pointCoords () =
  fenetre#utiliser_pointeur_creer ();
  change_state POINT_CLIQUE;
  print_message "State : Point coords.";
  let coords = Input_coords.window() in    
    match coords with
	None -> ()
      | Some c -> 
	  begin
	    let p = (new objet_point_clique c) in
	      initialise_objet p;
	      redessine_figure () 
	  end

let pointMilieu () =
  fenetre#utiliser_pointeur_creer ();
  change_state POINT_MILIEU_1;
  print_message "State : Midpoint."

let pointSurDroite () = 
  fenetre#utiliser_pointeur_creer ();
  change_state POINT_SUR_DROITE;
  print_message "State : Point on line."

let pointSurCercle () =	
  fenetre#utiliser_pointeur_creer ();
  change_state POINT_SUR_CERCLE;
  print_message "State : Point on circle."

let pointIntersectionDroiteDroite () =
  fenetre#utiliser_pointeur_creer ();
  change_state INTERSECTION_DROITE_DROITE_1;
  print_message "State : Intersection of two lines."

let pointIntersectionCercleDroite () =
  fenetre#utiliser_pointeur_creer ();
  change_state INTERSECTION_CERCLE_DROITE_1;
  print_message "State : Intersection of a line and a circle."
	 
let pointIntersectionCercleCercle () =  
  fenetre#utiliser_pointeur_creer ();
  change_state INTERSECTION_CERCLE_CERCLE_1;
  print_message "State : Intersection of two circles."

let pointLeftTurn () =  
  fenetre#utiliser_pointeur_creer ();
  change_state POINT_LEFT_TURN_1;
  print_message "State : Left Turn."

let pointBetween () =  
  fenetre#utiliser_pointeur_creer ();
  change_state POINT_BETWEEN_1;
  print_message "State : Point on segment."

let droiteDeuxPoints () =
  fenetre#utiliser_pointeur_creer ();
  change_state DROITE_DEUX_POINT_1;
  print_message "State : line passing by two points."

let demieDroite () =
  fenetre#utiliser_pointeur_creer ();
  change_state DEMIE_DROITE_DEUX_POINT_1;
  print_message "State : half-line passing by two points."

let droiteMediatrice () =
  fenetre#utiliser_pointeur_creer ();
  change_state MEDIATRICE_1;
  print_message "State : Perpendicular bissector."

let droiteBissectrice () =
  fenetre#utiliser_pointeur_creer ();
  change_state BISSECTRICE_ANGLE_1;
  print_message "State : Angle bissector."

let segmentDeuxPoints () = 
  fenetre#utiliser_pointeur_creer ();
  change_state SEGMENT_1;
  print_message "State : Segment."

let vecteurDeuxPoints () =
  fenetre#utiliser_pointeur_creer ();
  change_state VECTEUR_1;
  print_message "State : Vector."

let droiteParallele () = 
  fenetre#utiliser_pointeur_creer ();
  change_state DROITE_PARALLELE_1;
  print_message
    "State : Parallèle à une droite passant par un point.
    Chosissez d'abord la droite."

    
let droiteOrthogonale () =
  fenetre#utiliser_pointeur_creer ();
  change_state DROITE_ORTHOGONALE_1;
  print_message "State : Perpendiculaire à une droite passant par un point.
    Chosissez d'abord la droite."	
    
let cercleCentrePoint () =
  fenetre#utiliser_pointeur_creer ();
  change_state CERCLE_CENTRE_POINT_1;
  print_message
    "State : Circle by center and point."

let cercleTroisPoints () =
  fenetre#utiliser_pointeur_creer ();
  change_state CERCLE_TROIS_POINTS_1;
  print_message
    "State : Circle defined by three points."


let cercleDiametre () = 
  fenetre#utiliser_pointeur_creer ();
  change_state CERCLE_DIAMETRE_1;
  print_message 
    "State : Circle defined by its diameter."

let translation () =  
  fenetre#utiliser_pointeur_creer ();
  change_state TRANSLATION_1;
  print_message
    "State : Translation. Choose the vector."

let symmetriePoint () =
  fenetre#utiliser_pointeur_creer ();
  change_state SYMETRIE_POINT_1;
  print_message
    "State : Symetrical point. Choose un point."	

let symmetrieDroite () =
  fenetre#utiliser_pointeur_creer ();
  change_state SYMETRIE_DROITE_1;
  print_message
    "State : Symetrical droite. Choose a line."

let test_alignement () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_ALIGNEMENT_1;
  print_message
    "State : Test de collinéarité. Choose the first point."

let test_points_confondus () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_POINTS_CONFONDUS_1;
  print_message
    "State : Test d'égalité. Choose the first point."

let test_parallelisme () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_PARALLELISME_1;
   print_message
    "State : Parallelism test. Choose the first line."

let test_perpendicularite () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_PERPENDICULARITE_1;
   print_message
    "State : Orthogonality test. Choose the first line."

let test_congruent_segments () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_CONGRUENT_SEGMENTS_1;
   print_message
    "State : Test if two segments are congruent. Choose the first segment."

let test_congruent_angles () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_CONGRUENT_ANGLES_1;
   print_message
    "State :  Test if two angles are congruent. Choose the first angle."

let test_left_turn () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_LEFT_TURN_1;
   print_message
    "State :  Test if a point is on the left of two points."

let test_between () =
  fenetre#utiliser_pointeur_creer ();
  change_state TEST_BETWEEN_1;
  print_message
    "State :  Test if a point is between two other points."

let mesure_angle () =
  fenetre#utiliser_pointeur_creer ();
  change_state MESURE_ANGLE_1;
  print_message
    "State : Measure of an angle. Choose the first point."

let mesure_distance () =
  fenetre#utiliser_pointeur_creer ();
  change_state MESURE_DISTANCE_1;
  print_message
    "State : Measure of a distance. Choose the first point."

let mesure_area () =
  fenetre#utiliser_pointeur_creer ();
  change_state MESURE_AREA_1;
  print_message
    "State : Measure the area of a triangle."

let deplacePoint() =
  fenetre#utiliser_pointeur_deplacer ();
  Construction.highlight_movable_objects ();
  redessine_figure();
  etat := BOUGE_ATTEND;
  print_message "State : Move a point."
	
let efface () =
  fenetre#utiliser_pointeur_effacer ();
  change_state GOMME_OBJET;
  print_message "State : Delete some object.
    Warning : every object that depends on it will be deleted as well."

let selectionObjet () =
  fenetre#utiliser_pointeur_selectionner ();
  change_state SELECTION_OBJET;
  print_message "State : Select some object."

let move_paper () =
  fenetre#utiliser_pointeur_move_paper ();
  change_state MOVE_PAPER;
  print_message "State : Move the figure."


let changer_nom_objet x () =
  let nouveau_nom =
    GToolbox.input_string 
      ~title:(i18n "Edit name") 
      ~cancel:(i18n "Cancel")
      ~text:x#objet_nom 
      (i18n "Name :")
  in
  match nouveau_nom with
    Some e -> x#nomme e
  | None -> ()        

	
let popup_dun_objet (x:Objets_graphiques.t_objet_graphique) = 
(*  Debug.pdb "entree dans popup d'un objet, a savoir :";
  Debug.pdb x#objet_nom; *)
  let aux f s b =
    Debug.pdb "Fonction aux dans popup d un objet";
    if b then  
      begin 
(*	Debug.pdb "do f s"; *)
	f s;
(*	Debug.pdb "done f s";*)
	redessine_figure();
(*	Debug.pdb "figure redessinée";*)
      end    
    else ()
(*      Debug.pdb "b faux"*)
  in
  let coul = aux x#change_couleur in
  let styl = aux x#change_style in
  let epais = aux x#change_epaisseur in
  let couch = aux x#change_couche in
  let est_coul c = x#objet_couleur = c in
  let est_style c =  x#objet_style = c in
  let est_epaiss c = x#objet_epaisseur = c in
  let est_couche c = x#objet_couche = c in
  let unecoul t c = ((i18n t),est_coul c, coul c) in 
  let unstyle t c = ((i18n t),est_style c, styl c) in
  let uneepaiss t c = ((i18n t),est_epaiss c, epais c)   in
  let unecouche c = (Couche.couches.(c-1).nom,est_couche c, couch c)  in
  let rec liste_couches nb = 
    if nb = 0 then [] else (liste_couches (nb-1))@[(unecouche nb)] in

  let lesstyles =  
    match x#objet_style with
      STYLE_DROITE _ ->  
	[
	 unstyle "Normal" (STYLE_DROITE PLEINE);
	 unstyle "Dash" (STYLE_DROITE POINTILLEE)
       ]
    | STYLE_POINT _ ->
	[
	 unstyle "Cross" (STYLE_POINT CROIX);
	 unstyle "Circle" (STYLE_POINT ROND);
	 unstyle "Filled circle" (STYLE_POINT RONDPLEIN);
	 unstyle "Square" (STYLE_POINT CARRE);
	 unstyle "Filled square" (STYLE_POINT CARREPLEIN)
       ]
    | STYLE_REPERE _ ->
	[
	 unstyle "Show grid" (STYLE_REPERE {grid_shown=false});
	 unstyle "Hide grid" (STYLE_REPERE {grid_shown=true});
       ]
    | _ -> [] in

  let proof_proposition =
    match x#objet_type with
      TYPE_TEXTE | TYPE_MARK ->
	(
	 let pred = x#predicate_form () in
	 match pred with
	   None -> []
	 | Some formula ->
	     let str =  
	       Fol.string_printer formula;
	       Format.flush_str_formatter ()
	     in
	     let atp_entry =
	       (`I ((i18n "Prove that ")^str^(i18n " using embedded ATP"),
		    (Automatic_proofs.create_window pred))) 
	     in
	     let coq_entry = 
	       if Coq_proofs.is_started () then
		 [(`I ((i18n "Prove that ")^str^(i18n " using Coq"),
		       (Coq_proofs.output_coq_for_goal formula)))]
	       else [] 
	     in
	     atp_entry::coq_entry
	)
    | _ -> []
  in  
 [
   (`I ((String.concat "" [(x#objet_nom);" :"]),(fun () -> ())));
   `S;
   (`I ((i18n "Rename"),(changer_nom_objet x)));
   (`I ((i18n "Hide"),(fun () -> x#cache_objet;redessine_figure())));
   (`I ((if x#locked then (i18n "Unlock") else (i18n "Lock")),
	(fun () -> if x#locked then x#unlock_object else x#lock_object)));
   (`I ((if x#traced then (i18n "Untrace") else (i18n "Trace")),
	(fun () -> if x#traced then x#untrace_object else x#trace_object)));
   (`I ((i18n "Delete"),(fun () -> (supprimme_objet x);redessine_figure())));
   (`M ((i18n "Color"),[(`R [
			  (* ("titi",false,fun b -> x#change_couleur blue;redessine_figure()); *)
			   unecoul "Black" Couleurs.black;
			   unecoul "Blue" Couleurs.blue;
			   unecoul "Grey" Couleurs.grey;
			   unecoul "Red" Couleurs.red;
			   unecoul "Orange" Couleurs.orange;
			   unecoul "Purple" Couleurs.purple;
			   unecoul "Pink" Couleurs.pink;
			   unecoul "Green" Couleurs.green;
			   unecoul "Yellow" Couleurs.yellow
			 ])]));
  begin
    match x#objet_type with
      TYPE_TEXTE -> (`I ((i18n "Edit text"),(Window_expressions.edit_expression x redessine_figure)))
    | _ ->
	(`M ((i18n "Style"),[ (`R lesstyles)]))
  end;
  (`M ((i18n "Width"),[
	`R(
	 [
	 uneepaiss "Thin" 1;
	 uneepaiss "Normal" 2;
	 uneepaiss "Thick" 3;
	 uneepaiss "Very thick" 4
       ])
      ]));
   (`M ((i18n "Layer"),[
	(`R 
	   (liste_couches !!nombre_de_couches))
      ]))
 ]@proof_proposition

  
let bouton3_click gev =
(*  Debug.pdb "Clique droit";*)
  choisis_objet_gen Construction.objets_proches gev (TYPE_REPERE::tout_type_objet) [] 
    (
     fun x ->
       let listeentree = (popup_dun_objet x) in
       GToolbox.popup_menu
	 ~entries:listeentree
	 ~button:(GdkEvent.Button.button gev)
	 ~time:(GdkEvent.Button.time gev)
    )

let bouton2_click _ =
(* Middle_click cycle trough selection, move and last creation tools...*)
  match !etat with
  | NONE -> () 
  | BOUGE_ATTEND | BOUGE_BOUGE _ ->  
      fenetre#utiliser_pointeur_creer ();
      change_state !last_create_state 
  | GOMME_OBJET -> ()
  | SELECTION_OBJET ->  deplacePoint()
  | e ->  
      last_create_state := e; 
      selectionObjet()
	

let callback_souris_click ev =
(*  Debug.pdb "click";*)
  begin
    match GdkEvent.Button.button ev with
    | 1 ->
	if GdkEvent.get_type ev = `BUTTON_PRESS then
	  bouton1_click ev;
	false
    | 2 -> 
	if GdkEvent.get_type ev = `BUTTON_PRESS then
	  bouton2_click ev;
	false
    | 3 ->
	if GdkEvent.get_type ev = `BUTTON_PRESS then
	  bouton3_click ev;
	false
    | n -> Debug.pdb ("bouton "^(string_of_int n)); false
  end

  
let callback_souris_declick ev =
  (* Debug.pdb "declic";*)
  match !etat with
  | BOUGE_BOUGE point ->
      begin
	match GdkEvent.Button.button ev with
	| 1 ->
	    if GdkEvent.get_type ev = `BUTTON_RELEASE then
	      begin
		let c = 
		  Repere.ecran_vers_papier
		    {x = of_float (GdkEvent.Button.x ev); y = of_float (GdkEvent.Button.y ev)} ()
		in
		Construction.bouger_objet point c;
	       	redessine_figure ();
		etat := BOUGE_ATTEND;
		false
	      end
	    else 
	      false	
	| _ -> false
      end
  
  | MOVE_PAPER_2 starting_point ->
      begin
	match GdkEvent.Button.button ev with
	  | 1 ->
	      if GdkEvent.get_type ev = `BUTTON_RELEASE then
		begin
		  let end_point =
		    {x = of_float (GdkEvent.Button.x ev); y = of_float (GdkEvent.Button.y ev)} 
		  in
		    Repere.move_paper starting_point end_point ();
	       	    redessine_figure ();
		    etat := MOVE_PAPER;
		    false
		end
	      else 
		false	
	  | _ -> false
      end

  | _ -> false
	

let callback_souris_drag ev =
  (*Debug.pdb "drag";*)
  (* 
     This functions draw the figure again only if we have the time to. 
     We want to keep the interactivity. 
  *)
  let current_position () =
    let aux (x,y) = (of_int x, of_int y) in
    let (x,y) = 
      if GdkEvent.Motion.is_hint ev 
      then 
	aux fenetre#document_gui.area#misc#pointer
      else
	(of_float (GdkEvent.Motion.x ev), of_float (GdkEvent.Motion.y ev))
    in
      {x=x;y=y}
  in
    match !etat with
      | BOUGE_BOUGE point ->
	  let c = Repere.ecran_vers_papier (current_position ()) () in      
	  let state = GdkEvent.Motion.state ev in
	    if Gdk.Convert.test_modifier `BUTTON1 state then
	      begin	  
		Construction.bouger_objet point c;
		redessine_figure ()
	      end;
	    false
      | MOVE_PAPER_2 start ->   
	  let p = current_position () in
	  let state = GdkEvent.Motion.state ev in
	    if Gdk.Convert.test_modifier `BUTTON1 state then
	      begin
		etat := MOVE_PAPER_2 p;
		Repere.move_paper start p ();
		redessine_figure();
	      end;
	    false
      | _ -> 
	  let l,exceptions = interresting_objects !etat in
	  let near = 
	    Construction.objets_proches 
	      (Repere.get()) 
	      (current_position ()) 
	      l 
	      exceptions
	  in
	    if not (Construction.filter (fun x -> x#highlighted) = near) then
	      begin
		Construction.unhighlight_objects NEAR ();	    
		List.iter (fun x -> x#highlight_object NEAR) near;
		redessine_figure();
	      end;
	    false
	  

let callback_expose _ =
  (* Instead of drawing again the whole figure we just put the pixmap *) 
  fenetre#document_gui.drawing#put_pixmap ~x:0 ~y:0 fenetre#document_gui.drawingbuffer#pixmap;
  false

let callback_configure ev =
  (* 
     This call back is used when the window is resized for example.
  *)
  Repere.change_screen_width (GdkEvent.Configure.width ev);
  Repere.change_screen_height (GdkEvent.Configure.height ev);
  redessine_figure ();
  false

    
let export_to_generic function_to_apply  window_name complete () =
  let fs = GWindow.file_selection
    ~modal: true
    ~title:window_name
    () in
    
  let traiter_ok () = 
    function_to_apply fs#filename;
    fs#destroy ()
  in
    
  fs#connect#destroy ~callback:GMain.Main.quit;
  fs#ok_button#connect#clicked ~callback:traiter_ok;
  fs#cancel_button#connect#clicked ~callback:fs#destroy;
  fs#set_select_multiple false;
  fs#set_show_fileops true;
  fs#complete complete;
  fs#show();
  GMain.Main.main()

let export_to_bitmap () =
  let typ =
    match 
      GToolbox.question_box
	~title:(i18n "In which format do you want to save the picture ?")
	~buttons:["jpeg";"bmp";"png"]
	~default:3
	(i18n "In which format do you want to save the picture ?")
    with
      1 -> "jpeg"
    | 2 -> "bmp"
    | 3 -> "png"
    | _ -> assert false
  in
  let function_to_apply s =
    let photo = Rsvg.render_from_string (Output_svg.to_string ()) in
    GdkPixbuf.save ~filename:s ~typ:typ photo
  in
  export_to_generic function_to_apply ("Save to "^typ) ("*."^typ) ()
 
let export_to_svg () =
  export_to_generic  Output_svg.to_file "Save to SVG" "*.svg" ()
 
let export_to_kig () =
  GToolbox.message_box ~title:"Not implemented" "Not implemented yet" 
 
(*
  export_to_generic Output_kig.to_file "Save to Kig" "*.kig" ()
*)
let export_to_car () =
  GToolbox.message_box ~title:"Not implemented" "Not implemented yet" 
    
(*
  export_to_generic  Output_car.to_file "Save to Car" "*.zir" ()
*)

let export_to_ps () =
  GToolbox.message_box ~title:"Not implemented" "Not implemented yet" 
 
let export_to_eukleides () =
  let include_latex_header = 
    GToolbox.question_box 
      ~title:(i18n "Include latex header ?") 
      ~buttons:[(i18n "Latex");(i18n "Eukleide")] 
      ~default:1 
      (i18n 
	 "The figure will be translated into the eukleides language for use in a latex file. This file should be preprocessed using eukleides.\nFor more information about eukleides see http://perso.wanadoo.fr/obrecht/\n\nDo you want to produce a complete latex file (ready to compile) or just the eukleides snippet ?")
  in
  export_to_generic  (Output_eukleides.to_file (include_latex_header = 1)) 
    "Save to Eukleides format for latex output" "*.tex" ()

let export_to_pst_eucl () =
  GToolbox.message_box ~title:"Not implemented" "Not implemented yet" 
(*
  export_to_generic  Output_pst_eucl.to_file "Save to Pst-Eucl (pstricks) format for latex output" "*.tex" ()
*)

let deletePage () =
  let confirmation =
    GToolbox.question_box 
      ~title:(i18n "New figure") 
      ~buttons:[(i18n "Delete all");(i18n "Cancel")] 
      ~default:2 
   (i18n "Do you really want to delete all ?")
  in
  if confirmation = 1 then
    begin
      (* On efface tout *)
      Document_managment.delete_current_page();
      reset_name_generation();
      Document_managment.update_after_save_or_open ();
      redessine_figure()
    end


let pleinEcran () =
  if  fenetre#itemPleinEcran#active 
  then 
    begin
      fenetre#window#fullscreen ();
    end
  else 
    begin
      fenetre#window#unfullscreen ();      
    end;
  redessine_figure ()

(* Fonctions de selection *)

let fairePourTous f =  
  Construction.iter f;redessine_figure ()
 
let toutRendreVisible () = 
  fairePourTous (fun x -> x#montre_objet)

let selectionTout () =
  fairePourTous (fun x -> x#selectionne_objet) 

let selectionAucune () =
  fairePourTous (fun x -> x#deselectionne_objet) 

let selectionInverser () = 
  fairePourTous 
    (fun x -> 
      if x#objet_selectionne 
      then x#deselectionne_objet 
      else x#selectionne_objet
    )

let selectionConditionnelle pred =
  fairePourTous (fun x -> if (pred x) then x#selectionne_objet) 

let selectionToutVisible () = selectionConditionnelle (fun x -> x#objet_visible)
let selectionToutCache () =  selectionConditionnelle (fun x -> not x#objet_visible)

let selectionUnType untype = 
    selectionConditionnelle (fun x -> x#objet_type = untype)

let selectionDroites () = selectionUnType TYPE_DROITE
let selectionSegments () =  selectionUnType TYPE_SEGMENT
let selectionPoints () = selectionUnType TYPE_POINT
let selectionVecteurs () = selectionUnType TYPE_VECTEUR
let selectionCercles () = selectionUnType TYPE_CERCLE
let selectionLibres () = selectionConditionnelle (fun x -> x#movable())

    
(* Fonctions de changements d'attributs *)

let selectionRealiseAction f = 
  fairePourTous (fun x -> if (x#objet_selectionne) then f x) 

let supprimerSelection () = 
  selectionRealiseAction Document_managment.remove_object

let selectionChangeCouleur c =  
  selectionRealiseAction (fun x -> x#change_couleur c)

let selectionChangeEpaisseur e =  
  selectionRealiseAction (fun x -> x#change_epaisseur e)

let selectionChangeStyleDroite e =  
  selectionRealiseAction (fun x -> x#change_style (STYLE_DROITE e))

let selectionChangeCouche c =
  selectionRealiseAction (fun x -> x#change_couche c)

let selectionChangeVisibilite b =
  selectionRealiseAction (fun x -> x#change_visibilite b)

(* Menu couches *)

let coucheChangeVisibilite c =
  Couche.couches.(c-1).cache <- not Couche.couches.(c-1).cache

let coucheChangeNom c =
  let nouveau_nom =
    GToolbox.input_string 
      ~title:(i18n "Change layer's name") 
      ~cancel:(i18n "Cancel")
      ~text:(Couche.couches.(c-1).nom) 
      ((i18n "Name")^" :")
  in
    match nouveau_nom with
	Some e ->  
	  Couche.couches.(c-1).nom <- e; (* We change the name of the layer *)
	  (* We update the menus *)
	  let change_text labels_array = labels_array.(c-1)#set_text e in
	    change_text fenetre#itemLayersLabelsSelection;
	    change_text fenetre#itemLayersLabelsDefaut;
	    change_text fenetre#itemLayersLabels
      | None -> ()        
	  
let coucheChangeCouleur c col =
  Couche.couches.(c-1).couleur <- col


let defautChangeCouleur c = couleur_defaut =:= c
let defautChangeCouleurPoint c = couleur_point_defaut =:= c
let defautChangeEpaisseur e = epaisseur_defaut =:= e 
let defautChangeCouche c = couche_defaut =:= c
let defautChangeStyleDroite s = style_droite_defaut =:= s
let defautChangeStylePoint s = style_point_defaut =:= s
let fondChangeCouleur c = couleur_fond =:= c

let unitéDistanceChange u = unité_distance =:= u
let unitéAngleChange u = unité_angle =:= u



let current_filename = ref None

	
(* Save as *)

let choisir_et_enregistrer_fichier () = 
  let default d = function
    | None -> d
    | Some v -> v
  in
	
  (* We move to the last opened folder *)
  List.iter Unix.chdir !!last_folder;
	
  let fs = GWindow.file_selection
      ~modal: true
      ~title:"Save File"
      () in
  
  let traiter_ok () = 
    (* We update the last opened folder *)
    last_folder =:= value_to_path (filename_to_value (Filename.dirname fs#filename));
    (* We update the information to detect changes *)
    Document_managment.update_after_save_or_open ();
    current_filename := Some fs#filename;
    Output_drg.to_file fs#filename;
    fs#destroy ()
  in
  
  fs#connect#destroy ~callback:GMain.Main.quit;
  fs#ok_button#connect#clicked ~callback:traiter_ok;
  fs#cancel_button#connect#clicked ~callback:fs#destroy;
  fs#set_select_multiple false;
  fs#set_show_fileops true;
  fs#complete "*.drg";
  fs#show();
  GMain.Main.main()

(* Save *)

let save () =
  match !current_filename with 
    None ->  choisir_et_enregistrer_fichier ()
  | Some filename -> Output_drg.to_file ~overwrite:true filename

let quitter () = 
  if Document_managment.has_changed_since_last_saved_or_opened () then
    let confirmation =
      GToolbox.question_box 
	~title:(i18n "Save file ?") 
	~buttons:[(i18n "Discard changes");(i18n "Save");(i18n "Cancel")] 
	~default:3 
	(i18n "The current file has been modified, do you want to save it ?")
    in
      match confirmation with
	  1 -> (Options.save_with_help Optionsdeconfiguration.config_ini; 
		GMain.Main.quit ())
	| 2 -> (choisir_et_enregistrer_fichier ();
		Options.save_with_help Optionsdeconfiguration.config_ini; 
		GMain.Main.quit ())
	| _ -> ()
  else
    (Options.save_with_help Optionsdeconfiguration.config_ini; 
     GMain.Main.quit ())
    
let update_recent_files filename =
(* If the file is already in the recent files list we do nothing *)
  if (not (List.mem filename !!recent_files_list)) then
    begin
      if List.length !!recent_files_list < !!number_of_recent_files then
	recent_files_list =:= filename::!!recent_files_list
      else
	recent_files_list =:= List.rev (List.tl (List.rev (filename :: !!recent_files_list)));
      (* We update the menu *)
      let update_item i item = item#set_text ((string_of_int (i+1))^": "^Filename.basename (List.nth !!recent_files_list i)) in
      Array.iteri update_item fenetre#items_recent_files_labels
    end

let open_file filename = 
  if Filename.check_suffix filename ".geo" then
    Input_langue_naturelle.entree_langue_naturelle filename
  else 
    Input_xml.entree_xml filename
	

(* Ouvrir fichier *)
let choisir_et_ouvrir_fichier () = 
  let really_choose_and_open_file () =
    (*
      let nb = Document_managment.new_document() in
      fenetre#add_document_tab nb ();
    *)
    reset_name_generation();
    redessine_figure();     
    
    let default d = 
      function
	| None -> d
	| Some v -> v
    in
    
    (* We move to the last opened folder *)
    List.iter Unix.chdir !!last_folder;
    
    let fs = GWindow.file_selection
	~modal: true
	~title:"Open File"
	() 
    in
    
    let traiter_ok () = 
      (* We update the last opened folder *)
      last_folder =:= value_to_path (filename_to_value (Filename.dirname fs#filename));
      (* We update the list of recent files *)
      update_recent_files fs#filename;
      (* We update the information to detect changes *)
      Document_managment.update_after_save_or_open ();
      current_filename := Some (fs#filename);
      open_file fs#filename;
      fs#destroy ();
      redessine_figure()
    in
    
    fs#connect#destroy ~callback:GMain.Main.quit;
    fs#ok_button#connect#clicked ~callback:traiter_ok;
    fs#cancel_button#connect#clicked ~callback:fs#destroy;
    fs#set_select_multiple false;
    fs#set_show_fileops true;
    fs#show();
    GMain.Main.main()
  in
  
  if Document_managment.has_changed_since_last_saved_or_opened() then
    let confirmation =
      GToolbox.question_box 
	~title:(i18n "Open figure") 
	~buttons:[(i18n "Discard changes");(i18n "Save");(i18n "Cancel")] 
	~default:3 
	(i18n "The current file has been modified, do you want to save it ?")
    in
    match confirmation with
      1 -> really_choose_and_open_file ()
    | 2 -> choisir_et_enregistrer_fichier ()
    | _ -> ()
  else
    really_choose_and_open_file ()	

let evenement_clavier key =
  match key with
  | "\027" ->  aucunEtat () (* Touche Escape, change l'état en NONE, *)	  
  | "P" | "p" -> pointLibre ()
  | "T" | "t" -> texteLibre ()
  | "M" | "m" -> pointMilieu ()
  | "O" | "o" -> pointSurDroite ()
  | "U" | "u" -> pointSurCercle () 
  | "I" | "i" -> pointIntersectionDroiteDroite ()
  | "E" | "e" -> pointIntersectionCercleDroite ()
  | "F" | "f" -> pointIntersectionCercleCercle ()
  | "D" | "d" -> droiteDeuxPoints ()
  | "A" | "a" -> demieDroite ()
  | "S" | "s" -> segmentDeuxPoints ()
  | "V" | "v" -> vecteurDeuxPoints ()
  | "L" | "l" -> droiteParallele ()
  | "H" | "h" -> droiteOrthogonale ()
  | "C" | "c" -> cercleCentrePoint ()
  | "Q" | "q" -> cercleDiametre ()
  | "J" | "j" -> translation ()
  | "Y" | "y" -> symmetriePoint ()
  | "Z" | "z" -> symmetrieDroite ()
  | "B" | "b" -> deplacePoint ()
  | "G" | "g" -> efface ()
  | "+" -> Repere.zoomAvant (); redessine_figure ()
  | "-" -> Repere.zoomArriere (); redessine_figure ()
  | "/" -> Construction.zoom_ajuste (); redessine_figure ()
  | "2" -> Repere.deplaceHaut (); redessine_figure ()
  | "8" -> Repere.deplaceBas (); redessine_figure ()
  | "4" -> Repere.deplaceGauche (); redessine_figure ()
  | "6" -> Repere.deplaceDroite () ;redessine_figure ()
  | _ -> ()
 

let callback_clavier ev =
  evenement_clavier (GdkEvent.Key.string ev);false

let handler1 = ref None 

let do_unbox f o = match o with
  Some e -> f e
| None -> ()

let change_current_doc n =
  Debug.pdb ("Change_current_doc "^(string_of_int n));
  Debug.pdb (string_of_int !Document_managment.current_document_nb);

  do_unbox fenetre#document_gui.notebook#misc#disconnect !handler1;


  Document_managment.change_current_document n;

  fenetre#document_gui.area#event#add
    [`BUTTON_PRESS;`POINTER_MOTION;`POINTER_MOTION_HINT;`BUTTON_RELEASE];
     
  fenetre#document_gui.svg_event_box#event#add 
    [`BUTTON_PRESS;`POINTER_MOTION;`POINTER_MOTION_HINT;`BUTTON_RELEASE];

  Debug.pdb "put handler ";
  handler1 := Some (fenetre#document_gui.notebook#connect#switch_page 
		      ~callback:redraw_tab);
(*
  Debug.pdb "remove handler";
  do_unbox fenetre#document_gui.notebook#misc#disconnect !handler1;
  Debug.pdb "done";
*)

  fenetre#document_gui.area#event#connect#configure 
    ~callback:callback_configure;
  fenetre#document_gui.area#event#connect#expose 
    ~callback:callback_expose;
  fenetre#document_gui.area#event#connect#key_press 
    ~callback:callback_clavier;
  fenetre#document_gui.area#event#connect#button_press 
    ~callback:callback_souris_click;
  fenetre#document_gui.area#event#connect#button_release 
    ~callback:callback_souris_declick;
  fenetre#document_gui.area#event#connect#motion_notify 
    ~callback:callback_souris_drag; 
  
  fenetre#document_gui.svg_event_box#event#connect#button_press
    ~callback:callback_souris_click;
  fenetre#document_gui.svg_event_box#event#connect#key_press 
    ~callback:callback_clavier;
  fenetre#document_gui.svg_event_box#event#connect#button_release 
    ~callback:callback_souris_declick;
  fenetre#document_gui.svg_event_box#event#connect#motion_notify 
    ~callback:callback_souris_drag; 
  redessine_figure ()


let newDocument () =
  let nb = Document_managment.new_document () in
  fenetre#add_document_tab nb ();
  fenetre#notebookdocs#goto_page nb;
 (* change_current_document nb; *)
  (*  Document_managment.update_after_save_or_open (); *)
  redessine_figure()

let coq_start_proof () =
  (* Change the text of the menu entry *)
  (* TODO *)

  (* Show only the allowed constructions *) 
  let unused_buttons lang =
    match lang with
      Narboux ->
	[
	 fenetre#boutonPointCercle;
	 fenetre#boutonPointInterCercleDroite;
	 fenetre#boutonPointInterCercles;
	 fenetre#boutonPointLeftTurn;
	 fenetre#boutonPointBetween;
	 fenetre#boutonDemieDroite;
	 fenetre#boutonDroiteMediatrice;
	 fenetre#boutonDroiteBissectrice;
	 fenetre#boutonDroiteVecteur;
	 fenetre#boutonDroiteOrthogonale;
	 fenetre#boutonTestPerpendicularite;
	 fenetre#boutonTestCongruentAngles;
	 fenetre#boutonTestBetween;
	 fenetre#boutonTestLeftTurn;
	 fenetre#boutonTestCongruentSegments
       ] 	 
    | Guilhot -> 	 
	[
	  fenetre#boutonPointBetween;
	  fenetre#boutonPointLeftTurn;
	  fenetre#boutonTestBetween;
	  fenetre#boutonTestLeftTurn;
	]
  in
  let unused_bars lang =
    match lang with
      Narboux ->
	[
	 fenetre#barreoutilsCercles;
	 fenetre#barreoutilsTransformations
       ] 
    | Guilhot -> []
  in  
  
  let hide l = 
    List.iter (fun o -> o#misc#set_sensitive false) l 
  in
  
  let unhide l = 
    List.iter (fun o -> o#misc#set_sensitive true) l 
  in

  if not (Coq_proofs.is_started ()) then
    begin
      (* We start *)
      (* First We prevent from changing the language *)
      fenetre#itemFormalLanguage#misc#set_sensitive false;
      (* Then we hide the some buttons *)
      begin
	match !!coq_geom_language with
	  Narboux -> 
	    hide (unused_buttons Narboux);
	    hide (unused_bars Narboux) 	  
	| Guilhot -> 
	    hide (unused_buttons Guilhot)
      end; 
      (* Then start the proof *)
      Coq_proofs.start_proof ()
    end
  else
    begin
      (* First We allow changing the language *)
      fenetre#itemFormalLanguage#misc#set_sensitive true;
      begin
	match !!coq_geom_language with
	  Narboux -> 
	    unhide (unused_buttons Narboux);
	    unhide (unused_bars Narboux) 	  
	| Guilhot -> 
	    unhide (unused_buttons Guilhot)
      end; 
      (* We stop the proof *)
       Coq_proofs.stop_proof ()
    end

let initialise () = 
  
  let _  =  
    
    (* fenetre#itemCache#set_active (not !!visibilité_barre_statut); *) 
    if (not !!visibilité_barre_statut) then fenetre#statusbar#misc#hide ()
  in

  let _ = fenetre#notebookdocs#connect#switch_page ~callback:change_current_doc in
  let _ = fenetre#window#event#connect#expose ~callback:callback_expose in

  let _ = change_current_doc 0 in
    
 
(*
  let _ = fenetre#document_gui.area#event#add [`BUTTON_PRESS;`POINTER_MOTION;`POINTER_MOTION_HINT;`BUTTON_RELEASE] in
     
  let _ = fenetre#document_gui.svg_event_box#event#add [`BUTTON_PRESS;`POINTER_MOTION;`POINTER_MOTION_HINT;`BUTTON_RELEASE] in
  
  let _ = fenetre#document_gui.notebook#connect#switch_page ~callback:redraw_tab in

  let _ = fenetre#document_gui.area#event#connect#configure ~callback:callback_configure in

  let _ = fenetre#document_gui.area#event#connect#expose ~callback:callback_expose in
 
  let _ = fenetre#document_gui.area#event#connect#key_press ~callback:callback_clavier in
  let _ = fenetre#document_gui.area#event#connect#button_press ~callback:callback_souris_click in
  let _ = fenetre#document_gui.area#event#connect#button_release ~callback:callback_souris_declick in
  let _ = fenetre#document_gui.area#event#connect#motion_notify ~callback:callback_souris_drag in 

  let _ = fenetre#document_gui.svg_event_box#event#connect#button_press ~callback:callback_souris_click in
  let _ = fenetre#document_gui.svg_event_box#event#connect#key_press ~callback:callback_clavier in
  let _ = fenetre#document_gui.svg_event_box#event#connect#button_release ~callback:callback_souris_declick in
  let _ = fenetre#document_gui.svg_event_box#event#connect#motion_notify ~callback:callback_souris_drag in 
*)

  (* menu Fichiers *)
  let _ = fenetre#itemNouveau#connect#activate ~callback:deletePage in
  let _ = fenetre#itemQuitter#connect#activate ~callback:quitter in
  let _ = fenetre#itemSaveAs#connect#activate ~callback:choisir_et_enregistrer_fichier in
  let _ = fenetre#itemSave#connect#activate ~callback:save in

  let _ = fenetre#itemBitmap#connect#activate ~callback:export_to_bitmap in
  let _ = fenetre#itemSvg#connect#activate ~callback:export_to_svg in
  let _ = fenetre#itemKig#connect#activate ~callback:export_to_kig in
  let _ = fenetre#itemCar#connect#activate ~callback:export_to_car in
  let _ = fenetre#itemPs#connect#activate ~callback:export_to_ps in
  let _ = fenetre#itemEukleides#connect#activate ~callback:export_to_eukleides in
  let _ = fenetre#itemPstEucl#connect#activate ~callback:export_to_pst_eucl in

  let _ = fenetre#itemOuvrir#connect#activate ~callback:choisir_et_ouvrir_fichier in

  let cree_callback i item = ignore (item#connect#activate ~callback:
     (fun () -> try open_file (List.nth !!recent_files_list i) with _ -> ())) in
  let _ = Array.iteri cree_callback fenetre#items_recent_files in

  (* menu Edition *)

  let _ = fenetre#itemDefaire#connect#activate ~callback:defaire_derniere_action in
  let _ = fenetre#itemRefaire#connect#activate ~callback:refaire_derniere_action in

  let _ = fenetre#itemDeleteTraces#connect#activate ~callback:delete_traces in

  let _ = fenetre#itemSelectionnerTout#connect#activate ~callback:selectionTout in
  let _ = fenetre#itemSelectionnerToutVisible#connect#activate ~callback:selectionToutVisible in
  let _ = fenetre#itemSelectionnerToutCache#connect#activate ~callback:selectionToutCache in

  let _ = fenetre#itemSelectionDroites#connect#activate ~callback:selectionDroites in
  let _ = fenetre#itemSelectionSegments#connect#activate ~callback:selectionSegments in

  let _ = fenetre#itemSelectionCercles#connect#activate ~callback:selectionCercles in
  let _ = fenetre#itemSelectionPoints#connect#activate ~callback:selectionPoints in
  let _ = fenetre#itemSelectionVecteurs#connect#activate ~callback:selectionVecteurs in
  let _ = fenetre#itemSelectionLibres#connect#activate ~callback:selectionLibres in
  let _ = fenetre#itemSelectionAucune#connect#activate ~callback:selectionAucune in
  let _ = fenetre#itemSelectionInverser#connect#activate ~callback:selectionInverser in
  let _ = fenetre#itemSelectionObjet#connect#activate ~callback:selectionObjet in
  let _ = fenetre#itemSupprimerSelection#connect#activate ~callback:supprimerSelection in
  let _ = fenetre#itemToutRendreVisible#connect#activate ~callback:toutRendreVisible in


  (* menu Points *)
  let _ = fenetre#itempointclique#connect#activate ~callback:pointLibre in
  let _ = fenetre#itempointcoords#connect#activate ~callback:pointCoords in
  let _ = fenetre#itempointmilieu#connect#activate ~callback:pointMilieu in
  let _ = fenetre#itempointCercle#connect#activate ~callback:pointSurCercle in
  let _ = fenetre#itempointDroite#connect#activate ~callback:pointSurDroite in
  let _ = fenetre#itempointbetween#connect#activate ~callback:pointBetween in
  let _ = fenetre#itempointleftturn#connect#activate ~callback:pointLeftTurn in
  let _ = fenetre#itemintersectiondroites#connect#activate  ~callback:pointIntersectionDroiteDroite in
  let _ = fenetre#itemintersectioncercledroite#connect#activate ~callback:pointIntersectionCercleDroite in
  let _ = fenetre#itemintersectioncercles#connect#activate ~callback:pointIntersectionCercleCercle in
  (* menu Droites *)
  let _ = fenetre#itemdroitedeuxpoints#connect#activate  ~callback:droiteDeuxPoints in
  let _ = fenetre#itemhalfline#connect#activate  ~callback:demieDroite in
  let _ = fenetre#itemdroitemediatrice#connect#activate  ~callback:droiteMediatrice in
  let _ = fenetre#itemdroitebissectrice#connect#activate  ~callback:droiteBissectrice in

  let _ = fenetre#itemsegment#connect#activate ~callback:segmentDeuxPoints in
  let _ = fenetre#itemvecteur#connect#activate ~callback:vecteurDeuxPoints in
  let _ = fenetre#itemdroiteparallele#connect#activate ~callback:droiteParallele in
  let _ = fenetre#itemdroiteorthogonale#connect#activate ~callback:droiteOrthogonale in
  (* menu Cercles *)
  let _ = fenetre#itemcerclecentrepoint#connect#activate ~callback:cercleCentrePoint in
  let _ = fenetre#itemcercletroispoints#connect#activate ~callback:cercleTroisPoints in
  let _ = fenetre#itemcerclediametre#connect#activate ~callback:cercleDiametre in
  (* menu Transformations *)
  let _ = fenetre#itemtranslation#connect#activate ~callback:translation in
  let _ = fenetre#itemsymetriepoint#connect#activate ~callback:symmetriePoint in
  let _ = fenetre#itemsymetriedroite#connect#activate ~callback:symmetrieDroite in

  (* menu Mesures *)
  let _ = fenetre#itemtestalignement#connect#activate ~callback:test_alignement in
  let _ = fenetre#itemtestpointsconfondus#connect#activate ~callback:test_points_confondus in
  let _ = fenetre#itemtestparallelisme#connect#activate ~callback:test_parallelisme in
  let _ = fenetre#itemtestperpendicularite#connect#activate ~callback:test_perpendicularite in
  let _ = fenetre#itemtestbetween#connect#activate ~callback:test_between in
  let _ = fenetre#itemtestleftturn#connect#activate ~callback:test_left_turn in
  let _ = fenetre#itemmesureangle#connect#activate ~callback:mesure_angle in
  let _ = fenetre#itemmesuredistance#connect#activate ~callback:mesure_distance in
  let _ = fenetre#itemmesurearea#connect#activate ~callback:mesure_area in

  (* menu Labels *)
  let _ = fenetre#itemfreelabel#connect#activate ~callback:texteLibre in
  let _ = fenetre#itemlabelpoint#connect#activate ~callback:textePoint in


  (* menu Tools *)
  let _ = fenetre#itemdeplacepoint#connect#activate  ~callback:deplacePoint in
  let _ = fenetre#itemgommeobjet#connect#activate ~callback:efface in

  (* menu Proofs *) 

  let _ = fenetre#itemATP#connect#activate ~callback:(Automatic_proofs.create_window None) in
  let _ = fenetre#itemCoq#connect#activate ~callback:(Coq_proofs.copy_to_clipboard_statement None) in
  let _ = fenetre#itemCoqIde#connect#activate ~callback:(Coq_proofs.copy_to_coq_ide_statement None) in
  let _ = fenetre#itemCoqInteractive#connect#activate ~callback:(coq_start_proof) in

  let _ = fenetre#itemNarboux#connect#activate ~callback:(fun () -> coq_geom_language =:= Narboux) in
  let _ = fenetre#itemGuilhot#connect#activate ~callback:(fun () -> coq_geom_language =:= Guilhot) in

 (* menu Attributs *)
  let _ = fenetre#itemRouge#connect#activate ~callback:(fun () -> selectionChangeCouleur red) in
  let _ = fenetre#itemVert#connect#activate ~callback:(fun () -> selectionChangeCouleur green) in
  let _ = fenetre#itemJaune#connect#activate ~callback:(fun () -> selectionChangeCouleur yellow) in
  let _ = fenetre#itemBleu#connect#activate ~callback:(fun () -> selectionChangeCouleur blue) in
  let _ = fenetre#itemOrange#connect#activate ~callback:(fun () -> selectionChangeCouleur orange) in
  let _ = fenetre#itemViolet#connect#activate ~callback:(fun () -> selectionChangeCouleur purple) in
  let _ = fenetre#itemGris#connect#activate ~callback:(fun () -> selectionChangeCouleur grey) in
  let _ = fenetre#itemNoir#connect#activate ~callback:(fun () -> selectionChangeCouleur black) in
  let _ = fenetre#itemRose#connect#activate ~callback:(fun () -> selectionChangeCouleur pink) in

  let _ = fenetre#itemEpaisseur1#connect#activate ~callback:(fun () -> (selectionChangeEpaisseur 1)) in
  let _ = fenetre#itemEpaisseur2#connect#activate ~callback:(fun () -> (selectionChangeEpaisseur 2)) in
  let _ = fenetre#itemEpaisseur3#connect#activate ~callback:(fun () -> (selectionChangeEpaisseur 3)) in
  let _ = fenetre#itemEpaisseur4#connect#activate ~callback:(fun () -> (selectionChangeEpaisseur 4)) in
  
  let _ = fenetre#itemPlein#connect#activate ~callback:(fun () -> (selectionChangeStyleDroite PLEINE)) in
  let _ = fenetre#itemPointille#connect#activate ~callback:(fun () -> (selectionChangeStyleDroite POINTILLEE)) in

  let cree_callback i item = ignore (item#connect#activate ~callback:(fun () -> (selectionChangeCouche (i+1)))) in
  let _ = Array.iteri cree_callback fenetre#itemCoucheSelection in

  let _ = fenetre#itemCache#connect#activate 
      ~callback:(fun () -> selectionChangeVisibilite (not fenetre#itemCache#active)) in

  (* menu Vue *)
  let _ = fenetre#itemZoomAvant#connect#activate ~callback:(fun () -> Repere.zoomAvant();redessine_figure ()) in
  let _ = fenetre#itemZoomArriere#connect#activate ~callback:(fun () -> Repere.zoomArriere();redessine_figure ()) in
  let _ = fenetre#itemZoomAjuste#connect#activate ~callback:(fun () -> Construction.zoom_ajuste();redessine_figure ()) in

  let _ = fenetre#itemShowGrid#connect#activate  	
    ~callback:(fun () -> 
		 (List.hd Document_managment.initial_constructions)#change_visibilite 
		   (not (List.hd Document_managment.initial_constructions)#objet_visible);
		 redessine_figure ()) 
  in 

  let _ = fenetre#itemPleinEcran#connect#activate  ~callback:pleinEcran in 
  let _ = fenetre#window#event#connect#delete ~callback:(fun _ -> quitter();true) in
    
  let _ = fenetre#window#show () in

  (* menu Couches *)

  let cree_callback i item = 
    ignore (item.itemCache#connect#activate 
	      ~callback:(fun () -> (coucheChangeVisibilite (i+1))));
    ignore (item.itemNom#connect#activate 
	      ~callback:(fun () -> (coucheChangeNom (i+1))));
    ignore (item.itemCouleurAucune#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) None)));
    ignore (item.itemCouleurJaune#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.yellow))));
    ignore (item.itemCouleurRouge#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.red))));
    ignore (item.itemCouleurNoir#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.black))));
    ignore (item.itemCouleurGris#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.grey))));
    ignore (item.itemCouleurOrange#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.orange))));
    ignore (item.itemCouleurViolet#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.purple))));
    ignore (item.itemCouleurRose#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.pink))));
    ignore (item.itemCouleurBleu#connect#activate 
	      ~callback:(fun () -> (coucheChangeCouleur (i+1) (Some Couleurs.blue))))
  in
  
  let _ = Array.iteri cree_callback fenetre#itemCouches in

  (* menu Configuration *)

  let _ = fenetre#itemRougeFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur red;redessine_figure()) in
  let _ = fenetre#itemVertFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur green;redessine_figure()) in
  let _ = fenetre#itemJauneFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur yellow;redessine_figure()) in
  let _ = fenetre#itemBleuFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur blue;redessine_figure()) in
  let _ = fenetre#itemOrangeFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur orange;redessine_figure()) in
  let _ = fenetre#itemVioletFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur purple;redessine_figure()) in
  let _ = fenetre#itemGrisFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur grey;redessine_figure()) in
  let _ = fenetre#itemBlancFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur white;redessine_figure()) in
  let _ = fenetre#itemRoseFond#connect#activate 
      ~callback:(fun () -> fondChangeCouleur pink;redessine_figure()) in

  let _ = fenetre#itemRougeDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur red) in
  let _ = fenetre#itemVertDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur green) in
  let _ = fenetre#itemJauneDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur yellow) in
  let _ = fenetre#itemBleuDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur blue) in
  let _ = fenetre#itemOrangeDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur orange) in
  let _ = fenetre#itemVioletDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur purple) in
  let _ = fenetre#itemGrisDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur grey) in
  let _ = fenetre#itemNoirDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur black) in
  let _ = fenetre#itemRoseDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleur pink) in

  let _ = fenetre#itemRougePointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint red) in
  let _ = fenetre#itemVertPointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint green) in
  let _ = fenetre#itemJaunePointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint yellow) in
  let _ = fenetre#itemBleuPointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint blue) in
  let _ = fenetre#itemOrangePointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint orange) in
  let _ = fenetre#itemVioletPointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint purple) in
  let _ = fenetre#itemGrisPointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint grey) in
  let _ = fenetre#itemNoirPointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint black) in
  let _ = fenetre#itemRosePointDefaut#connect#activate 
      ~callback:(fun () -> defautChangeCouleurPoint pink) in

  let _ = fenetre#itemEpaisseur1Defaut#connect#activate 
      ~callback:(fun () -> (defautChangeEpaisseur 1)) in
  let _ = fenetre#itemEpaisseur2Defaut#connect#activate 
      ~callback:(fun () -> (defautChangeEpaisseur 2)) in
  let _ = fenetre#itemEpaisseur3Defaut#connect#activate 
      ~callback:(fun () -> (defautChangeEpaisseur 3)) in
  let _ = fenetre#itemEpaisseur4Defaut#connect#activate 
      ~callback:(fun () -> (defautChangeEpaisseur 4)) in

  let _ = fenetre#itemPleinDefaut#connect#activate 
      ~callback:(fun () -> (defautChangeStyleDroite PLEINE)) in
  let _ = fenetre#itemPointilleDefaut#connect#activate 
      ~callback:(fun () -> (defautChangeStyleDroite POINTILLEE)) in

  let _ = fenetre#itemCroixDefaut#connect#activate 
      ~callback:(fun () -> (defautChangeStylePoint CROIX)) in
   let _ = fenetre#itemRondDefaut#connect#activate 
      ~callback:(fun () -> (defautChangeStylePoint ROND)) in
  let _ = fenetre#itemRondPleinDefaut#connect#activate 
      ~callback:(fun () -> (defautChangeStylePoint RONDPLEIN)) in
  let _ = fenetre#itemCarreDefaut#connect#activate 
      ~callback:(fun () -> (defautChangeStylePoint CARRE)) in
  let _ = fenetre#itemCarrePleinDefaut#connect#activate 
      ~callback:(fun () -> (defautChangeStylePoint CARREPLEIN)) in


  let cree_callback i item = ignore (item#connect#activate ~callback:(fun () -> (defautChangeCouche i))) in
  let _ = Array.iteri cree_callback fenetre#itemCoucheDefaut in

  let  _ = fenetre#itemUnitéCms#connect#activate 
      ~callback:(fun () -> (unitéDistanceChange CMS)) in
  let  _ = fenetre#itemUnitéPouces#connect#activate 
      ~callback:(fun () -> (unitéDistanceChange POUCES)) in
  let  _ = fenetre#itemUnitéPoints#connect#activate 
      ~callback:(fun () -> (unitéDistanceChange POINTS)) in
 

  let  _ = fenetre#itemUnitéDegres#connect#activate 
      ~callback:(fun () -> (unitéAngleChange DEGRES)) in
  let  _ = fenetre#itemUnitéRadians#connect#activate 
      ~callback:(fun () -> (unitéAngleChange RADIANS)) in
 

  let cache_element t u confvar () = 
    if t#active 
    then 
      begin
	confvar =:= false;
	u#misc#hide ()
      end 
    else
      begin
	confvar =:= true;
	u#misc#show () 
      end
  in

  let _ = fenetre#itemBarEtat#connect#activate 
      ~callback:(cache_element fenetre#itemBarEtat fenetre#statusbar  visibilité_barre_statut) in

  let _ = fenetre#itemBarrePrincipale#connect#activate 
      ~callback:(cache_element fenetre#itemBarrePrincipale fenetre#barreoutilsPrinc visibilité_barre_outils_principale) in
  let _ = fenetre#itemBarreVue#connect#activate 
      ~callback:(cache_element fenetre#itemBarreVue fenetre#barreoutilsVue visibilité_barre_outils_vue) in
  let _ = fenetre#itemBarrePoints#connect#activate 
      ~callback:(cache_element fenetre#itemBarrePoints fenetre#barreoutilsPoints visibilité_barre_outils_point) in
  let _ = fenetre#itemBarreDroites#connect#activate 
      ~callback:(cache_element fenetre#itemBarreDroites fenetre#barreoutilsDroites visibilité_barre_outils_droite) in
  let _ = fenetre#itemBarreCercles#connect#activate 
      ~callback:(cache_element fenetre#itemBarreCercles fenetre#barreoutilsCercles visibilité_barre_outils_cercle) in
  let _ = fenetre#itemBarreTransformations#connect#activate 
      ~callback:(cache_element fenetre#itemBarreTransformations fenetre#barreoutilsTransformations visibilité_barre_outils_transformation) in
  let _ = fenetre#itemBarreFacts#connect#activate 
      ~callback:(cache_element fenetre#itemBarreFacts fenetre#barreoutilsFacts visibilité_barre_outils_facts) in
  let _ = fenetre#itemBarreMeasures#connect#activate 
      ~callback:(cache_element fenetre#itemBarreMeasures fenetre#barreoutilsMeasures visibilité_barre_outils_measures) in
  let _ = fenetre#itemBarreLabels#connect#activate 
      ~callback:(cache_element fenetre#itemBarreLabels fenetre#barreoutilsLabels visibilité_barre_outils_labels) in


  (* menu Aide *)
  let _ = fenetre#itemApropos#connect#activate  ~callback:Apropos.window_about in

  (* Boutons Vue *)

  let _ = fenetre#boutonPleinEcran#connect#clicked  
      ~callback:(fun () -> 
	fenetre#itemPleinEcran#set_active (not fenetre#itemPleinEcran#active);
	pleinEcran()) 
  in

  let _ = fenetre#boutonZoomAvant#connect#clicked ~callback:(fun () -> Repere.zoomAvant();redessine_figure ()) in
  let _ = fenetre#boutonZoomArriere#connect#clicked ~callback:(fun () -> Repere.zoomArriere();redessine_figure ()) in
  let _ = fenetre#boutonZoomAjuste#connect#clicked ~callback:(fun () -> Construction.zoom_ajuste();redessine_figure ()) in
  let _ = fenetre#boutonMovePaper#connect#clicked ~callback:(fun () -> move_paper();redessine_figure ()) in

  let _ =  fenetre#boutonRepere#connect#clicked 
	~callback:(fun () -> 
		     (List.hd Document_managment.initial_constructions)#change_visibilite 
		       (not (List.hd Document_managment.initial_constructions)#objet_visible);
		     redessine_figure ()) 
  in

  (* Boutons Points *)
  let _ = fenetre#boutonPointLibre#connect#clicked ~callback:pointLibre in
  ignore (fenetre#boutonPointCoords#connect#clicked 
	    ~callback:(
	  fun () -> 
	    (* we open the window only when the button is pressed (not released) *)
	    if fenetre#boutonPointCoords#active 
	    then pointCoords () 
	    else ()));
  let _ = fenetre#boutonPointMilieu#connect#clicked ~callback:pointMilieu in
  let _ = fenetre#boutonPointCercle#connect#clicked ~callback:pointSurCercle in
  let _ = fenetre#boutonPointDroite#connect#clicked ~callback:pointSurDroite in

  let _ = fenetre#boutonPointInterDroites#connect#clicked  ~callback:pointIntersectionDroiteDroite in
  let _ = fenetre#boutonPointInterCercleDroite#connect#clicked ~callback:pointIntersectionCercleDroite in
  let _ = fenetre#boutonPointInterCercles#connect#clicked ~callback:pointIntersectionCercleCercle in
  let _ = fenetre#boutonPointLeftTurn#connect#clicked ~callback:pointLeftTurn in
  let _ = fenetre#boutonPointBetween#connect#clicked ~callback:pointBetween in
(*  let _ = fenetre#boutonATP#connect#clicked ~callback:(Automatic_proofs.create_window None) in
  let _ = fenetre#boutonCoq#connect#clicked ~callback:(Coq_proofs.create_window None) in
*)

  (* Boutons Droites *)
  let _ = fenetre#boutonDroiteDeuxPoints#connect#clicked  ~callback:droiteDeuxPoints in
  let _ = fenetre#boutonDemieDroite#connect#clicked  ~callback:demieDroite in
  let _ = fenetre#boutonDroiteMediatrice#connect#clicked  ~callback:droiteMediatrice in
  let _ = fenetre#boutonDroiteBissectrice#connect#clicked  ~callback:droiteBissectrice in

  let _ = fenetre#boutonDroiteSegment#connect#clicked ~callback:segmentDeuxPoints in
  let _ = fenetre#boutonDroiteVecteur#connect#clicked ~callback:vecteurDeuxPoints in
  let _ = fenetre#boutonDroiteParallele#connect#clicked ~callback:droiteParallele in
  let _ = fenetre#boutonDroiteOrthogonale#connect#clicked ~callback:droiteOrthogonale in
  (* Boutons Cercles *)
  let _ = fenetre#boutonCercleCentrePoint#connect#clicked ~callback:cercleCentrePoint in
  let _ = fenetre#boutonCercleTroisPoints#connect#clicked ~callback:cercleTroisPoints in
  let _ = fenetre#boutonCercleDiametre#connect#clicked ~callback:cercleDiametre in
  (* Boutons Transformations *)
  let _ = fenetre#boutonTranslation#connect#clicked ~callback:translation in
  let _ = fenetre#boutonSymetriePoint#connect#clicked ~callback:symmetriePoint in
  let _ = fenetre#boutonSymetrieDroite#connect#clicked ~callback:symmetrieDroite in


  (* Boutons Mesures *)
  let _ = fenetre#boutonTestAlignement#connect#clicked ~callback:test_alignement in
  let _ = fenetre#boutonTestPointsConfondus#connect#clicked ~callback:test_points_confondus in
  let _ = fenetre#boutonTestParallelisme#connect#clicked ~callback:test_parallelisme in
  let _ = fenetre#boutonTestPerpendicularite#connect#clicked ~callback:test_perpendicularite in
  let _ = fenetre#boutonTestBetween#connect#clicked ~callback:test_between in
  let _ = fenetre#boutonTestLeftTurn#connect#clicked ~callback:test_left_turn in
  let _ = fenetre#boutonTestCongruentSegments#connect#clicked ~callback:test_congruent_segments in
  let _ = fenetre#boutonTestCongruentAngles#connect#clicked ~callback:test_congruent_angles in


  let _ = fenetre#boutonMesureAngle#connect#clicked ~callback:mesure_angle in
  let _ = fenetre#boutonMesureDistance#connect#clicked ~callback:mesure_distance in
  let _ = fenetre#boutonMesureArea#connect#clicked ~callback:mesure_area in

  (* Boutons Labels *)
  let _ = fenetre#boutonTexteClique#connect#clicked ~callback:texteLibre in
  let _ = fenetre#boutonTextePoint#connect#clicked ~callback:textePoint in
  

  (* Boutons Principaux *)

  let _ = fenetre#boutonNouveau#connect#clicked 
      ~callback:deletePage in
  let _ = fenetre#boutonOuvrir#connect#clicked 
      ~callback:choisir_et_ouvrir_fichier in
  let _ = fenetre#boutonSaveAs#connect#clicked 
      ~callback:choisir_et_enregistrer_fichier in
  let _ = fenetre#boutonSave#connect#clicked 
      ~callback:save in



  let _ = fenetre#boutonDeplacer#connect#clicked 
      ~callback:deplacePoint in
  let _ = fenetre#boutonSelectionnerObjet#connect#clicked 
      ~callback:selectionObjet in  
  let _ = fenetre#boutonDefaire#connect#clicked 
      ~callback:defaire_derniere_action in
  let _ = fenetre#boutonRefaire#connect#clicked 
      ~callback:refaire_derniere_action in
  
  let _ = fenetre#boutonInterrompre#connect#clicked ~callback:aucunEtat in
  let _ = fenetre#boutonSupprimer#connect#clicked ~callback:efface in
    (* This fonction tries in a loop to send the messages to Coqide *)
   ignore (Thread.create Coq_proofs.send_messages ());
    GtkThread.main ()
    (* use GtkThread.main because we are using threads in automatic theorem proving *)


    
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
  
