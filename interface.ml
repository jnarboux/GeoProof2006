(* $Id *)

open Couche
open Gdk
open Icons
open GDraw
open GMain
open I18n 
open Options
open Optionsdeconfiguration
open Repere
open Document_gui

type type_item_couche = 
    {
     itemNom : GMenu.menu_item; 
     itemCache : GMenu.check_menu_item;
     itemCouleurAucune : GMenu.radio_menu_item;
     itemCouleurVert : GMenu.radio_menu_item;	
     itemCouleurJaune : GMenu.radio_menu_item;
     itemCouleurRouge : GMenu.radio_menu_item;
     itemCouleurNoir : GMenu.radio_menu_item;
     itemCouleurGris : GMenu.radio_menu_item;
     itemCouleurOrange : GMenu.radio_menu_item;
     itemCouleurViolet : GMenu.radio_menu_item;
     itemCouleurRose : GMenu.radio_menu_item;
     itemCouleurBleu : GMenu.radio_menu_item;
   }
      
class fenetre_principale () =
  let icon = get_pixbuf icon_geoproof 48 48 in
  let window =
    GWindow.window ~title:(i18n Autoconf.big_name) ~icon ~allow_shrink:false ~height:600 ~allow_grow:true () in
  let vbox = GPack.vbox ~homogeneous:false ~packing:window#add () in
  let mbarprincipale = GMenu.menu_bar ~packing:(vbox#pack ~expand:false ~fill:true) () in
  
  (* Pointeurs de souris *)
  
  let pointeurDeplacer = Gdk.Cursor.create `FLEUR in
  let pointeurSelectionner =  Gdk.Cursor.create `ARROW in
  let pointeurCreer = Gdk.Cursor.create `PENCIL in
  let pointeurEffacer = Gdk.Cursor.create `TCROSS in
  let pointeurMovePaper = Gdk.Cursor.create `HAND2 in
 
 (*
  let mypixbuf = GdkPixbuf.from_file "eraser" in
  let mypixmap = GdkPixbuf.create_pixmap mypixbuf in
  let bitmap = match snd mypixmap with Some e -> e |_ -> assert false in
  let pointeurCreer =  Gdk.Cursor.create_from_pixmap  (fst mypixmap) 
    ~mask:(bitmap) 
    ~fg:(GDraw.color `BLACK)
    ~bg:(GDraw.color `WHITE) 
    ~x:0
    ~y:0
  in
 *)
 
  let utiliser_pointeur_deplacer () = Gdk.Window.set_cursor window#misc#window pointeurDeplacer in
  let utiliser_pointeur_creer () = Gdk.Window.set_cursor window#misc#window pointeurCreer in
  let utiliser_pointeur_selectionner () = Gdk.Window.set_cursor window#misc#window pointeurSelectionner in
  let utiliser_pointeur_effacer () = Gdk.Window.set_cursor window#misc#window pointeurEffacer in
  let utiliser_pointeur_move_paper () = Gdk.Window.set_cursor window#misc#window pointeurMovePaper in
  
  
    (* Création des icones *)
  let icon f = Icons.get_icon f !!icons_size in
  let small_icon f = Icons.get_icon f 15 in
  
  (* Barre de boutons *)
  let hbox = GPack.hbox ~spacing:10 ~packing:(vbox#pack ~expand:false) () in
  let hbox2 = GPack.hbox ~spacing:10 ~packing:(vbox#pack ~expand:false) () in
  let hbox3 = GPack.hbox ~spacing:10 ~packing:(vbox#pack ~expand:false) () in
  
  (* Boutons Principaux *)
  let handle_boxPrinc = GBin.handle_box ~packing:hbox#pack () in
  let barreoutilsPrinc = GButton.toolbar ~style:`ICONS ~packing:handle_boxPrinc#add () in 
  
  
(*MG essai factorisation *)
(*
  let button_make text icon_desc = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n text)
      ~tooltip:(i18n text)
      ~icon:(icon icon_desc ())
      ()
  in

  let buttonNew = button_make "New"  icon_filenew 
  in
  let buttonOpen = button_make "Open"  icon_fileopen
  in
  let buttonSave = button_make "Save"  icon_filesave
  in
  let buttonSaveAs = button_make "Save as ..."  icon_filesave (* as ?? *)
  in
*)
(*MG . . .  *)

  let boutonNouveau = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n "New")
      ~tooltip:(i18n "New")
      ~icon:(icon icon_filenew ())
      ()
  in


  let boutonOuvrir = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n "Open")
      ~tooltip:(i18n "Open")
      ~icon:(icon icon_fileopen ())
      ()
  in
    
  let boutonSave = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n "Save")
      ~tooltip:(i18n "Save")
      ~icon:(icon icon_filesave ())
      ()
  in

  let boutonSaveAs = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n "Save as...")
      ~tooltip:(i18n "Save as...")
      ~icon:(icon icon_filesave ())
      ()
  in

  let _ =  barreoutilsPrinc#insert_space () in

  let boutonRefaire = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n "Redo")
      ~tooltip:(i18n "Redo")
      ~icon:(icon icon_redo ())
      ()
  in
  
  let boutonDefaire = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n "Undo")
      ~tooltip:(i18n "Undo")
      ~icon:(icon icon_undo ())
      ()
  in
 
  let _ = barreoutilsPrinc#insert_space () in

  let boutonSelectionnerObjet = 
    barreoutilsPrinc#insert_radio_button 
      ~text:(i18n "Select")
      ~tooltip:(i18n "Select")
      ~icon:(icon icon_select ())
      ()
  in
  
  let boutonDeplacer = 
    barreoutilsPrinc#insert_radio_button 
      ~text:(i18n "Move")
      ~tooltip:(i18n "Move")
      ~icon:(icon icon_move ())
      ()
  in


(*
  let boutonDerniereConstruction = 
    barreoutilsPrinc#insert_radio_button 
      ~text:(i18n "Last construction")
      ~tooltip:(i18n "Last construction")
      ~icon:(icon "move" ())
      ()
  in
  let _ = boutonDerniereConstruction#set_group boutonSelectionnerObjet#group in
*)
 
  let boutonSupprimer = 
    barreoutilsPrinc#insert_radio_button 
      ~text:(i18n "Delete")
      ~tooltip:(i18n "Delete object")
      ~icon:(icon icon_eraser ())
      ()
  in

  let _ =  barreoutilsPrinc#insert_space () in

  let boutonInterrompre = 
    barreoutilsPrinc#insert_button 
      ~text:(i18n "Stop")
      ~tooltip:(i18n "Stop current construction")
      ~icon:(icon icon_stop ())
      ()
  in
  

  
    (* Boutons Vue *)
  let handle_boxVue = GBin.handle_box ~packing:hbox#pack () in
  let barreoutilsVue = GButton.toolbar ~style:`ICONS ~packing:handle_boxVue#add () in 
  let boutonZoomAvant = 
    barreoutilsVue#insert_button 
      ~text:(i18n "Zoom in")
      ~tooltip:(i18n "Zoom in")
      ~icon:(icon icon_viewmagplus ())
      ()
  in

  let boutonZoomArriere = 
    barreoutilsVue#insert_button 
      ~text:(i18n "Zoom out")
      ~tooltip:(i18n "Zoom out")
      ~icon:(icon icon_viewmagminus ())
      ()      
  in
 
  let boutonZoomAjuste = 
    barreoutilsVue#insert_button 
      ~text:(i18n "Zoom to fit")
      ~tooltip:(i18n "Zoom to fit")
      ~icon:(icon icon_viewmagfit ())
      ()      
  in

  let boutonMovePaper =
    barreoutilsVue#insert_button 
      ~text:(i18n "Move paper")
      ~tooltip:(i18n "Move paper")
      ~icon:(icon icon_pointing_finger ())
      ()      
  in


  let boutonPleinEcran = 
    barreoutilsVue#insert_toggle_button 
      ~text:(i18n "Full screen")
      ~tooltip:(i18n "Switch to full screen.")
      ~icon:(icon icon_window_fullscreen ())
      ()      
  in

  let boutonRepere =
    barreoutilsVue#insert_toggle_button 
      ~text:(i18n "Show/Hide grid")
      ~tooltip:(i18n "Show/Hide grid.")
      ~icon:(icon icon_grid ())
      ()      
  in

  (* Boutons Attributs *)
  (*
    let handle_boxAttributs = GBin.handle_box ~packing:hbox#pack () in
    let barreoutilsAttributs = GButton.toolbar ~style:`ICONS ~packing:handle_boxAttributs#add () in 
  *)
  (*
    let boutonCouleur = 
    barreoutilsAttributs#insert_button 
    ~text:(i18n "Color")
    ~tooltip:(i18n "Color")
    ~icon:(icon icon_viewmagplus ())
    ()
    in
    let boutonEpaisseur = 
    barreoutilsAttributs#insert_radio_button 
    ~text:(i18n "Width")
    ~tooltip:(i18n "Width")
    ~icon:(icon icon_viewmagplus ())
    ()
    in
  *)
  (*
    let comboEpaisseur = GEdit.combo ~packing:
    (barreoutilsAttributs#insert_widget
    ~tooltip:(i18n "Width")
    ~tooltip_private:(i18n  "Width")
    )
      ~popdown_strings:[(i18n "Fine");(i18n "Normal");(i18n "Thick");(i18n "Very thick")]
    ~value_in_list:true
    ~width:100
    () 
    in
    
    let _ = comboEpaisseur#entry#set_editable false in
  *)
    
    
    
  (* Boutons Points *)
    
  let handle_boxPoints = GBin.handle_box ~packing:hbox2#pack () in
  let barreoutilsPoints = GButton.toolbar ~style:`ICONS ~packing:handle_boxPoints#add () in  
    
  let boutonPointLibre = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Free point")
      ~tooltip:(i18n "Free point")
      ~icon:(icon icon_point ())
      ()
  in
  
  let boutonPointCoords = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Free point at some coordinates")
      ~tooltip:(i18n "Free point at some coordinates")
      ~icon:(icon icon_point_coords ())
      ()
  in

  let boutonPointCercle = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Point on circle")
      ~tooltip:(i18n "Point on circle")
      ~icon:(icon icon_pointcircle ())
      ()      
  in

  let boutonPointDroite = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Point on line")
      ~tooltip:(i18n "Point on line")
      ~icon:(icon icon_pointline ())
      ()      
  in
 
  let boutonPointMilieu = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Midpoint")
      ~tooltip:(i18n "Midpoint")
      ~icon:(icon icon_midpoint ())
      ()      
  in
  
  let boutonPointBetween = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Between")
      ~tooltip:(i18n "Between")
      ~icon:(icon icon_between ())
      ()      
  in
  
  let boutonPointInterDroites = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Lines intersection")
      ~tooltip:(i18n "Lines intersection")
      ~icon:(icon icon_intersection ())
      ()      
  in

  let boutonPointInterCercles = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Circles intersections")
      ~tooltip:(i18n "Circles intersections")
      ~icon:(icon icon_intersectionCircles ())
      ()      
  in

  let boutonPointInterCercleDroite = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Circle/Line intersections")
      ~tooltip:(i18n "Circle/Line intersections")
      ~icon:(icon icon_circlelineintersection ())
      ()      
  in

  let boutonPointLeftTurn = 
    barreoutilsPoints#insert_radio_button 
      ~text:(i18n "Left turn")
      ~tooltip:(i18n "Left turn")
      ~icon:(icon icon_left_turn ())
      ()      
  in

    (* Boutons Droites *)

  let handle_boxDroites = GBin.handle_box ~packing:hbox2#pack () in
  let barreoutilsDroites = GButton.toolbar ~style:`ICONS ~packing:handle_boxDroites#add () in  

  let boutonDroiteDeuxPoints = 
    barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Line, two points")
      ~tooltip:(i18n "Line, two points")
      ~icon:(icon icon_line ())
      ()
  in
 
 let boutonDemieDroite = 
    barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Half-Line, two points")
      ~tooltip:(i18n "Half-Line, two points")
      ~icon:(icon icon_half_line ())
      ()
  in

  let boutonDroiteSegment = 
    barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Segment, two points")
      ~tooltip:(i18n "Segment, two points")
      ~icon:(icon icon_segment ())
      ()
  in

  let boutonDroiteVecteur = 
    barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Vector, two points")
      ~tooltip:(i18n "Vector, two points")
      ~icon:(icon icon_vector ())
      ()
  in

  let boutonDroiteParallele = 
    barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Parallel line")
      ~tooltip:(i18n "Parallel line")
      ~icon:(icon icon_parallel ())
      ()
  in

  let boutonDroiteOrthogonale = 
    barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Orthogonal line")
      ~tooltip:(i18n "Orthogonal line")
      ~icon:(icon icon_perpendicular ())
      ()
  in

  let boutonDroiteMediatrice =
   barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Perpendicular bissector")
      ~tooltip:(i18n "Perpendicular bissector")
      ~icon:(icon icon_mediatrice ())
      ()
  in

  let boutonDroiteBissectrice =
   barreoutilsDroites#insert_radio_button 
      ~text:(i18n "Angle Bissector")
      ~tooltip:(i18n "Angle Bissector")
      ~icon:(icon icon_anglebisector ())
      ()
  in


    (* Boutons Cercles *)

  let handle_boxCercles = GBin.handle_box ~packing:hbox2#pack () in
  let barreoutilsCercles = GButton.toolbar ~style:`ICONS ~packing:handle_boxCercles#add () in  

  let boutonCercleCentrePoint = 
    barreoutilsCercles#insert_radio_button 
      ~text:(i18n "Circle, center point")
      ~tooltip:(i18n "Circle, center point")
      ~icon:(icon icon_circlebcp ())
      ()
  in

  let boutonCercleTroisPoints = 
    barreoutilsCercles#insert_radio_button 
      ~text:(i18n "Circle, three points")
      ~tooltip:(i18n "Circle, three points")
      ~icon:(icon icon_circlebtp ())
      ()
  in

  let boutonCercleDiametre = 
    barreoutilsCercles#insert_radio_button 
      ~text:(i18n "Circle, diameter")
      ~tooltip:(i18n "Circle, diameter")
      ~icon:(icon icon_circlediam ())
      ()
  in


    (* Boutons Transformations *)

  let handle_boxTransformations = GBin.handle_box ~packing:hbox2#pack () in
  let barreoutilsTransformations = GButton.toolbar ~style:`ICONS ~packing:handle_boxTransformations#add () in  

  let boutonSymetriePoint = 
    barreoutilsTransformations#insert_radio_button 
      ~text:(i18n "Central symmetry")
      ~tooltip:(i18n "Central symmetry")
      ~icon:(icon icon_centralsymmetry ())
      ()
  in
  let boutonSymetrieDroite = 
    barreoutilsTransformations#insert_radio_button 
      ~text:(i18n "Axial symetry")
      ~tooltip:(i18n "Axial symetry")
      ~icon:(icon icon_mirrorpoint ())
      ()
  in
  let boutonTranslation = 
    barreoutilsTransformations#insert_radio_button 
      ~text:(i18n "Translation")
      ~tooltip:(i18n "Translation")
      ~icon:(icon icon_translation ())
      ()
  in


  (* Boutons mesures *)

 
  let handle_boxTests = GBin.handle_box ~packing:hbox3#pack () in
  let barreoutilsTests = GButton.toolbar ~style:`ICONS ~packing:handle_boxTests#add () in  

  (* First the predicates : *)

  (* Teste si trois points sont alignés ou si un point appartient à une droite *)

  let boutonTestAlignement = 
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Collinear points")
      ~tooltip:(i18n "Collinear points")
      ~icon:(icon icon_testcollinear ())
      ()
  in

  (* Teste si deux points sont confondus *)
 
  let boutonTestPointsConfondus = 
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Equal points")
      ~tooltip:(i18n "Equal points")
      ~icon:(icon icon_testeqpoints ())
      ()
  in
  
(*
  (* Teste si trois droites sont concourantes *)

  let boutonDroitesConcourantes =
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Sequent lines")
      ~tooltip:(i18n "Sequent lines")
      ~icon:(icon icon_centralsymmetry ())
      ()
  in
*)

  (* Teste si deux droites sont perpendiculaires *)

  let boutonTestPerpendicularite =
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Perpendicular lines")
      ~tooltip:(i18n "Perpendicular lines")
      ~icon:(icon icon_testperpendicular ())
      ()
  in

  (* Tests if two segments are congruent *)

  let boutonTestCongruentSegments =
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Equal distances")
      ~tooltip:(i18n "Equal distances")
      ~icon:(icon icon_congruent_segments ())
      ()
  in 

  (* Tests if two angles are congruent *)

  let boutonTestCongruentAngles =
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Equal angles")
      ~tooltip:(i18n "Equal angles")
      ~icon:(icon icon_congruent_angles ())
      ()
  in 


  (* Teste si deux droites sont parallèles *)

   let boutonTestParallelisme =
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Parallel lines")
      ~tooltip:(i18n "Parallel lines")
      ~icon:(icon icon_testparallel ())
      ()
  in

   let boutonTestLeftTurn =
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Left Turn")
      ~tooltip:(i18n "Left Turn")
      ~icon:(icon icon_testleftturn ())
      ()
  in 

  let boutonTestBetween =
    barreoutilsTests#insert_radio_button 
      ~text:(i18n "Between")
      ~tooltip:(i18n "Between")
      ~icon:(icon icon_testbetween ())
      ()
  in
(*
  let boutonATP = 
   barreoutilsTests#insert_button 
      ~text:(i18n "Automatic Theorem Proving")
      ~tooltip:(i18n "Prove properties using an automatic theorem prover")
      ~icon:(icon icon_lightbulb ())
      ()
  in

  let boutonCoq = 
   barreoutilsTests#insert_button 
      ~text:(i18n "Interactive Thorem Proving using Coq")
      ~tooltip:(i18n "Prove properties using Coq")
      ~icon:(icon icon_rooster ())
      ()
  in
*)


   (* Des mesures *)

  let handle_boxMeasures = GBin.handle_box ~packing:hbox3#pack () in
  let barreoutilsMeasures = GButton.toolbar ~style:`ICONS ~packing:handle_boxMeasures#add () in  


   let boutonMesureAngle =
     barreoutilsMeasures#insert_radio_button 
       ~text:(i18n "Angle")
       ~tooltip:(i18n "Angle")
       ~icon:(icon icon_angle ())
       ()
   in

   let boutonMesureDistance =
     barreoutilsMeasures#insert_radio_button 
       ~text:(i18n "Distance")
       ~tooltip:(i18n "Distance")
       ~icon:(icon icon_distance ())
       ()
   in

  let boutonMesureArea =
    barreoutilsMeasures#insert_radio_button 
      ~text:(i18n "Triangle Area")
      ~tooltip:(i18n "Triangle Area")
      ~icon:(icon icon_area ())
      ()
   in

    (* Boutons labels *)

  let handle_boxLabels = GBin.handle_box ~packing:hbox3#pack () in
  let barreoutilsLabels = GButton.toolbar ~style:`ICONS ~packing:handle_boxLabels#add () in  

  (* Texte quelconque à l'endroit choisis à la souris *)

  let boutonTexteClique = 
    barreoutilsLabels#insert_radio_button 
      ~text:(i18n "Free text")
      ~tooltip:(i18n "Free text")
      ~icon:(icon icon_label ())
      ()
  in
  
  (* Texte qui se déplace avec un point *)
  
  let boutonTextePoint = 
    barreoutilsLabels#insert_radio_button 
      ~text:(i18n "Point label")
      ~tooltip:(i18n "Point label")
      ~icon:(icon icon_labelpoint ())
      ()
  in

  (* Met tous les boutons de création, deplacement, 
     suppr, selection dans le même groupe *)


  let _ = boutonDeplacer#set_group boutonSelectionnerObjet#group in
  let _ = boutonSupprimer#set_group boutonSelectionnerObjet#group in

  let _ = boutonPointLibre#set_group boutonSelectionnerObjet#group in 
  let _ = boutonPointCoords#set_group boutonSelectionnerObjet#group in 

  let _ = boutonPointDroite#set_group boutonSelectionnerObjet#group in
  let _ = boutonPointCercle#set_group boutonSelectionnerObjet#group in
  let _ = boutonPointMilieu#set_group boutonSelectionnerObjet#group in
  let _ = boutonPointInterDroites#set_group boutonSelectionnerObjet#group in
  let _ = boutonPointInterCercleDroite#set_group boutonSelectionnerObjet#group in
  let _ = boutonPointInterCercles#set_group boutonSelectionnerObjet#group in
  let _ = boutonPointLeftTurn#set_group boutonSelectionnerObjet#group in
  let _ = boutonPointBetween#set_group boutonSelectionnerObjet#group in

  let _ = boutonDroiteDeuxPoints#set_group boutonSelectionnerObjet#group in
  let _ = boutonDemieDroite#set_group boutonSelectionnerObjet#group in
  let _ = boutonDroiteVecteur#set_group boutonSelectionnerObjet#group in
  let _ = boutonDroiteSegment#set_group boutonSelectionnerObjet#group in
  let _ = boutonDroiteParallele#set_group boutonSelectionnerObjet#group in
  let _ = boutonDroiteOrthogonale#set_group boutonSelectionnerObjet#group in
  let _ = boutonDroiteMediatrice#set_group boutonSelectionnerObjet#group in
  let _ = boutonDroiteBissectrice#set_group boutonSelectionnerObjet#group in

  let _ = boutonCercleCentrePoint#set_group boutonSelectionnerObjet#group in
  let _ = boutonCercleTroisPoints#set_group boutonSelectionnerObjet#group in
  let _ = boutonCercleDiametre#set_group boutonSelectionnerObjet#group in
  
  let _ = boutonSymetriePoint#set_group boutonSelectionnerObjet#group in
  let _ = boutonSymetrieDroite#set_group boutonSelectionnerObjet#group in
  let _ = boutonTranslation#set_group boutonSelectionnerObjet#group in
  
  let _ = boutonTestAlignement#set_group boutonSelectionnerObjet#group in
  let _ = boutonTestPerpendicularite#set_group boutonSelectionnerObjet#group in
  let _ = boutonTestCongruentSegments#set_group boutonSelectionnerObjet#group in
  let _ = boutonTestCongruentAngles#set_group boutonSelectionnerObjet#group in
 
  let _ = boutonTestPointsConfondus#set_group boutonSelectionnerObjet#group in
  let _ = boutonTestParallelisme#set_group boutonSelectionnerObjet#group in
  let _ = boutonTestLeftTurn#set_group boutonSelectionnerObjet#group in
  let _ = boutonTestBetween#set_group boutonSelectionnerObjet#group in
  
(*
  let _ = boutonDroitesConcourantes#set_group boutonSelectionnerObjet#group in
*)
  
  let _ = boutonMesureArea#set_group boutonSelectionnerObjet#group in


  let _ = boutonMesureDistance#set_group boutonSelectionnerObjet#group in
  let _ = boutonMesureAngle#set_group boutonSelectionnerObjet#group in

  let _ = boutonTexteClique#set_group boutonSelectionnerObjet#group in
  let _ = boutonTextePoint#set_group boutonSelectionnerObjet#group in


  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false) () in

 (* Fonction to add an icon to an item in the menu *)
  let set_icon item icon_name () = item#set_image (small_icon icon_name ());item in

    (* menu Fichier *)
  let itemfichier = GMenu.menu_item ~label:(i18n "File") ~packing:mbarprincipale#add () in
  let menufichier = GMenu.menu ~packing:itemfichier#set_submenu () in
  let itemFichier s icon_name = set_icon (GMenu.image_menu_item ~label:(i18n s)  ~packing:menufichier#add ()) icon_name ()	
  in

  let itemNouveau = itemFichier "New" icon_filenew in
  let itemOuvrir = itemFichier "Open" icon_fileopen in
  let itemSave = itemFichier "Save" icon_filesave in
  let itemSaveAs = itemFichier "Save as..." icon_filesave in
(*
   let itemPrint = itemFichier "Print"  in
*)

  let _ = GMenu.menu_item  ~packing:menufichier#add () in 
  let itemexport = GMenu.menu_item ~label:(i18n "Export to") ~packing:menufichier#add () in
  let menuexport = GMenu.menu ~packing:itemexport#set_submenu () in
  let itemExport s b = GMenu.menu_item ~label:(i18n s) ~packing:menuexport#add ~show:b () in
  let itemBitmap = itemExport "Bitmap (.png or .jpeg or .bmp)" true in
  let itemSvg = itemExport "Scalable Vector Graphic (.svg)" true in
  let itemCar = itemExport "Compass and Ruler (.zir)" false in
  let itemKig = itemExport "KDE Interactive Geometry (.kig)" false in
  let itemPs = itemExport "Postscript (.ps)" false in
  let itemEukleides =  itemExport "Eukleides (.tex)" true in
  let itemPstEucl = itemExport "Pst-eucl (.tex)" false in


  let _ = GMenu.menu_item  ~packing:menufichier#add () in
  let itemRecentFileLabel i =  
    GMisc.label ~xalign:0.  ~text:(
    try (List.nth !!recent_files_list i) 
    with _ -> "Recent file") () 
  in
  let items_recent_files_labels = 
    Array.init !!number_of_recent_files itemRecentFileLabel in
  let itemRecentFile i =
    let item = GMenu.menu_item ~packing:menufichier#add () in
    item#add (items_recent_files_labels.(i)#coerce);
    item
      (* Perhaps the saved recent_files list is smaller than number_of_recent_files *) 
  in
  let items_recent_files =
    Array.init !!number_of_recent_files itemRecentFile in
  let _ = GMenu.menu_item  ~packing:menufichier#add () in
  let itemQuitter = itemFichier "Quit" icon_quit in

   (* menu Edition *)  
  let itemedition = GMenu.menu_item ~label:(i18n "Edit") ~packing:mbarprincipale#add () in
  let menuedition = GMenu.menu ~packing:itemedition#set_submenu () in
  let itemEdition s =  GMenu.menu_item ~label:(i18n s) ~packing:menuedition#add () in
  let itemEditionIcon s iconname = set_icon 
	(GMenu.image_menu_item ~label:(i18n s) ~packing:menuedition#add ()) iconname () in
  let itemDefaire =  itemEditionIcon "Undo last construction" icon_undo in
  let itemRefaire =  itemEditionIcon "Redo last construction" icon_redo in

  let _ = GMenu.menu_item  ~packing:menuedition#add () in 
  let itemDeleteTraces =  itemEdition "Delete Traces" in
  let _ = GMenu.menu_item  ~packing:menuedition#add () in 
  let itemSelectionnerTout = itemEdition "Select all" in
  let itemSelectionnerToutVisible = itemEdition "Select visible objects" in
  let itemSelectionnerToutCache = itemEdition "Select hiden objects" in
  let itemSelectionDroites = itemEdition "Select lines" in
  let itemSelectionSegments = itemEdition "Select segments" in
  let itemSelectionPoints = itemEdition "Select points" in
  let itemSelectionCercles = itemEdition "Select circles" in
  let itemSelectionVecteurs = itemEdition "Select vectors" in 
  let itemSelectionLibres = itemEdition "Select free objects" in 
  
  let _ = GMenu.menu_item  ~packing:menuedition#add () in 
  let itemSelectionObjet = itemEdition "Select object" in
  let _ = GMenu.menu_item  ~packing:menuedition#add () in
  let itemSelectionAucune = itemEdition "Select none" in
  let itemSelectionInverser = itemEdition "Invert selection" in
  let _ = GMenu.menu_item  ~packing:menuedition#add () in
  let itemToutRendreVisible = itemEdition "Show all objects" in
  let itemSupprimerSelection = itemEdition "Delete selected objects" in


    (* menu Attributs Selection *) 
  let _ = GMenu.menu_item  ~packing:menuedition#add () in 
  let itemattributs = GMenu.menu_item ~label:(i18n "Selection properties") ~packing:menuedition#add () in
  let menuattributs = GMenu.menu ~packing:itemattributs#set_submenu () in
  let itemAttributs s = GMenu.menu_item ~label:(i18n s) ~packing:menuattributs#add () in
  let itemCouleur = itemAttributs "Change color" in
  let menuCouleur = GMenu.menu ~packing:itemCouleur#set_submenu () in
  let itemCoul s = GMenu.menu_item ~label:(i18n s) ~packing:menuCouleur#add () in
  let itemRouge = itemCoul "Red" in
  let itemVert = itemCoul "Green" in
  let itemJaune = itemCoul "Yellow" in
  let itemBleu = itemCoul "Blue" in
  let itemGris = itemCoul "Grey" in
  let itemNoir = itemCoul "Black" in
  let itemOrange = itemCoul "Orange" in
  let itemViolet = itemCoul "Purple" in
  let itemRose = itemCoul "Pink" in

  let itemStyle = itemAttributs "Change style" in
  let menuStyle = GMenu.menu ~packing:itemStyle#set_submenu () in
  let itemStyl s = GMenu.menu_item ~label:(i18n s) ~packing:menuStyle#add () in
  let itemPlein = itemStyl "Solid" in
  let itemPointille = itemStyl "Dash" in

  let itemEpaisseur = itemAttributs "Change width" in
  let menuEpaisseur = GMenu.menu ~packing:itemEpaisseur#set_submenu () in
  let itemEpaiss s = GMenu.menu_item ~label:(i18n s) ~packing:menuEpaisseur#add () in
  let itemEpaisseur1 = itemEpaiss "Fine" in
  let itemEpaisseur2 = itemEpaiss "Normal" in
  let itemEpaisseur3 = itemEpaiss "Thick" in
  let itemEpaisseur4 = itemEpaiss "Very thick" in

  let itemCouche = itemAttributs "Change layer" in
  let menuCouche = GMenu.menu ~packing:itemCouche#set_submenu () in
  let itemCouch label = 
    let item = GMenu.menu_item ~packing:menuCouche#add () in
      item#add (label);
      item
  in

    (* Build an several arrays of labels for layers to be used in several menus, 
       we can not share the labels for some reason *)
    (* THe label will be updated when the layer's name changes *)

  let itemCoucheLabel i =  GMisc.label ~text:(Couche.couches.(i).nom) () in

  let itemLayersLabelsSelection =  Array.init !!nombre_de_couches itemCoucheLabel in
  let itemLayersLabelsDefaut =  Array.init !!nombre_de_couches itemCoucheLabel in
  let itemLayersLabels =  Array.init !!nombre_de_couches itemCoucheLabel in


  let itemCouche i = itemCouch (itemLayersLabelsSelection.(i)#coerce) in
  let itemCoucheSelection = Array.init !!nombre_de_couches itemCouche in

  let itemCache =  GMenu.check_menu_item ~label:(i18n "Invert visibility") ~packing:menuattributs#add () in
  
    (* menu Créer *)

  let itemcreer = GMenu.menu_item ~label:(i18n "Create") ~packing:mbarprincipale#add () in
  let menucreer = GMenu.menu ~packing:itemcreer#set_submenu () in

    (* menu Points *)
  let itempoints = GMenu.menu_item ~label:(i18n "Points") ~packing:menucreer#add () in
  let menupoints = GMenu.menu ~packing:itempoints#set_submenu () in
  let itemPoints s icon_name = set_icon (GMenu.image_menu_item ~label:(i18n s) ~packing:menupoints#add ()) icon_name () in
  let itempointclique = itemPoints "Free point" icon_point in
  let itempointcoords = itemPoints "Free point by coordinates" icon_point_coords in
  let itempointCercle = itemPoints "Point on circle" icon_pointcircle in
  let itempointDroite = itemPoints "Point on line" icon_pointline in
  let itempointmilieu = itemPoints "Midpoint" icon_midpoint in
  let itempointbetween = itemPoints "Between" icon_between in
  let itempointleftturn = itemPoints "Left Turn" icon_left_turn in

  let _ = GMenu.menu_item ~packing:menupoints#add () in
  let itemintersectiondroites = itemPoints "Lines intersection" icon_intersection in
  let itemintersectioncercledroite =itemPoints "Circle/Line intersections" icon_circlelineintersection in
  let itemintersectioncercles = itemPoints "Circles intersections" icon_intersectionCircles in

    (* menu Droites *)
  let itemdroites = GMenu.menu_item ~label:(i18n "Lines") ~packing:menucreer#add () in
  let menudroites = GMenu.menu ~packing:itemdroites#set_submenu () in
  let itemDroites s icon_name = set_icon (GMenu.image_menu_item ~label:(i18n s) ~packing:menudroites#add ()) icon_name () in
  let itemdroitedeuxpoints = itemDroites "Line, two points" icon_line in
  let itemhalfline = itemDroites "Half line, two points" icon_half_line in
  let itemsegment = itemDroites "Segment" icon_segment in
  let itemvecteur = itemDroites "Vector" icon_vector in
  let itemdroiteparallele = itemDroites "Parallel line" icon_parallel in
  let itemdroiteorthogonale = itemDroites "Orthogonal line" icon_perpendicular in
  let itemdroitemediatrice = itemDroites "Perpendicular bisector" icon_mediatrice in
  let itemdroitebissectrice =  itemDroites "Angle bissector" icon_anglebisector in

    (* Menu Cercles*)
  let itemcercles = GMenu.menu_item ~label:(i18n "Circles") ~packing:menucreer#add () in
  let menucercles = GMenu.menu ~packing:itemcercles#set_submenu () in
  let itemCercles s icon_name = set_icon (GMenu.image_menu_item ~label:(i18n s) ~packing:menucercles#add ()) icon_name () in
  let itemcerclecentrepoint = itemCercles "Circle, center point"  icon_circlebcp in
  let itemcercletroispoints = itemCercles "Circle, three points" icon_circlebtp in
  let itemcerclediametre = itemCercles "Circle, diameter" icon_circlediam in

    (* menu Transformations *)
  let itemtransformation = GMenu.menu_item ~label:(i18n "Transformations") ~packing:menucreer#add () in
  let menutransformation = GMenu.menu ~packing:itemtransformation#set_submenu () in
  let itemTransformation s icon_name = set_icon (GMenu.image_menu_item ~label:(i18n s) ~packing:menutransformation#add ()) icon_name () in
  let itemtranslation = itemTransformation "Translation" icon_translation in
  let itemsymetriepoint = itemTransformation "Central symetry" icon_centralsymmetry in
  let itemsymetriedroite = itemTransformation "Axial symetry" icon_mirrorpoint in
  

    (* menu Label *)
  let itemlabel = GMenu.menu_item ~label:(i18n "Labels") ~packing:menucreer#add () in
  let menulabel = GMenu.menu ~packing:itemlabel#set_submenu () in
  let itemLabel s icon_name = set_icon (GMenu.image_menu_item ~label:(i18n s) ~packing:menulabel#add ()) icon_name () in
  let itemfreelabel = itemLabel "Free label" icon_label in
  let itemlabelpoint = itemLabel "Point label" icon_labelpoint in

    (* menu Mesures *)

  let itemmesures = GMenu.menu_item ~label:(i18n "Tests and measures") ~packing:menucreer#add () in
  let menumesures = GMenu.menu ~packing:itemmesures#set_submenu () in
  let itemMesure s icon_name  = set_icon  (GMenu.image_menu_item ~label:(i18n s) ~packing:menumesures#add ()) icon_name () in
  let itemtestalignement = itemMesure "Collinearity" icon_testcollinear in
  let itemtestpointsconfondus =  itemMesure "Equality of points" icon_testeqpoints in
  let itemtestparallelisme =  itemMesure "Parallelism" icon_testparallel in
  let itemtestperpendicularite =  itemMesure "Orthogonality" icon_testperpendicular in
  let itemtestbetween =  itemMesure "Test betweeness"  icon_testbetween in
  let itemtestleftturn =  itemMesure "Test left-turn" icon_testleftturn in

  let itemmesureangle =  itemMesure "Measure distance" icon_distance in
  let itemmesuredistance = itemMesure "Measure angle" icon_angle in
  let itemmesurearea = itemMesure "Measure area" icon_area in

    (* menu Tools *)
  let itemtools = GMenu.menu_item ~label:(i18n "Tools") ~packing:mbarprincipale#add () in
  let menutools = GMenu.menu ~packing:itemtools#set_submenu () in
  let itemdeplacepoint = set_icon (GMenu.image_menu_item ~label:(i18n "Move point") ~packing:menutools#add ()) icon_move () in
  let itemgommeobjet = set_icon (GMenu.image_menu_item ~label:(i18n "Erase some object") ~packing:menutools#add ()) icon_eraser () in

    (* menu Proofs *)

  let itemproofs = GMenu.menu_item ~label:(i18n "Proofs") ~packing:mbarprincipale#add () in
  let menuproofs = GMenu.menu ~packing:itemproofs#set_submenu () in
  let itemATP = set_icon (GMenu.image_menu_item ~label:(i18n "Prove the statement corresponding to the current figure using the embedded automatic theorem prover") ~packing:menuproofs#add ()) icon_lightbulb () in
  let itemCoqInteractive = GMenu.check_menu_item ~label:(i18n "Start/Stop interactive proof using Coq") ~packing:menuproofs#add () in
  let itemCoqIde = set_icon (GMenu.image_menu_item ~label:(i18n "Paste the statement corresponding to the current figure into CoqIde") ~packing:menuproofs#add ()) icon_rooster () in
  let itemCoq = set_icon (GMenu.image_menu_item ~label:(i18n "Copy the statement corresponding to the current figure in Coq syntax into clipboard") ~packing:menuproofs#add ()) icon_rooster () in

  let itemFormalLanguage = GMenu.menu_item ~label:(i18n "Formal language used for communication with Coq") ~packing:menuproofs#add () in
  let menuFormalLanguage = GMenu.menu ~packing:itemFormalLanguage#set_submenu () in
  let itemNarboux = GMenu.radio_menu_item ~label:(i18n "Narboux") ~packing:menuFormalLanguage#add () in
  let itemGuilhot = GMenu.radio_menu_item ~group:itemNarboux#group ~label:(i18n "Guilhot") ~packing:menuFormalLanguage#add () in

  (* menu Couches *)
    
  let itemcouche = GMenu.menu_item ~label:(i18n "Layers") ~packing:mbarprincipale#add () in
  let menucouche = GMenu.menu ~packing:itemcouche#set_submenu () in
    
  let cree_un_item i = 
    let itemCouchei = 
      let item = GMenu.menu_item  ~packing:menucouche#add () in
	item#add  (itemLayersLabels.(i)#coerce);
	item
    in


    let menucouchei =  GMenu.menu ~packing:itemCouchei#set_submenu () in
    let itemCoucheiNom =  GMenu.menu_item ~label:(i18n "Rename") ~packing:(menucouchei)#add () in
    let itemCoucheiCachee =  GMenu.check_menu_item ~label:(i18n "Hiden") ~packing:menucouchei#add () in
    let itemCoucheiCouleur =  GMenu.menu_item ~label:(i18n "Color") ~packing:menucouchei#add () in
    let menuCoucheiCouleur = GMenu.menu ~packing:itemCoucheiCouleur#set_submenu () in
    let itemCoucheiCouleurAucune =  GMenu.radio_menu_item ~label:(i18n "None") ~packing:menuCoucheiCouleur#add () in
    let itemcoucheiCouleur s =  GMenu.radio_menu_item 
	~group:itemCoucheiCouleurAucune#group 
	~label:(i18n s) 
	~packing:menuCoucheiCouleur#add () in
    let itemCoucheiCouleurVert =  itemcoucheiCouleur "Green" in 
    let itemCoucheiCouleurJaune =  itemcoucheiCouleur "Yellow" in 
    let itemCoucheiCouleurRouge =  itemcoucheiCouleur "Red" in
    let itemCoucheiCouleurNoir =  itemcoucheiCouleur "Black" in
    let itemCoucheiCouleurGris =  itemcoucheiCouleur "Grey" in 
    let itemCoucheiCouleurOrange =  itemcoucheiCouleur "Orange" in
    let itemCoucheiCouleurViolet =  itemcoucheiCouleur "Purple" in
    let itemCoucheiCouleurRose =  itemcoucheiCouleur "Pink" in 
    let itemCoucheiCouleurBleu =  itemcoucheiCouleur "Blue" in
    {
     itemNom = itemCoucheiNom; 
     itemCache = itemCoucheiCachee;
     itemCouleurAucune = itemCoucheiCouleurAucune;
     itemCouleurVert = itemCoucheiCouleurVert;	
     itemCouleurJaune = itemCoucheiCouleurJaune;
     itemCouleurRouge =  itemCoucheiCouleurRouge;
     itemCouleurNoir = itemCoucheiCouleurNoir;
     itemCouleurGris = itemCoucheiCouleurGris;
     itemCouleurOrange = itemCoucheiCouleurOrange;
     itemCouleurViolet = itemCoucheiCouleurViolet;
     itemCouleurRose = itemCoucheiCouleurRose;
     itemCouleurBleu = itemCoucheiCouleurBleu
   }
  in
  
  let itemCouches = Array.init !!nombre_de_couches cree_un_item   in
    
  (* menu Vue *) 
  let itemvue = GMenu.menu_item ~label:(i18n "View") ~packing:mbarprincipale#add () in
  let menuvue = GMenu.menu ~packing:itemvue#set_submenu () in
  let itemVue s icon_name = set_icon (GMenu.image_menu_item ~label:(i18n s) ~packing:menuvue#add ()) icon_name () in
    
  (*  let itemZoomAvant2 = GMenu.image_menu_item ~label:("zoom avant 2") ~stock:`ZOOM_IN ~packing:menuvue#add () in *)
    
  let itemZoomAvant = itemVue "Zoom in" icon_viewmagplus in
  let itemZoomArriere = itemVue "Zoom out" icon_viewmagminus in
  let itemZoomAjuste = itemVue "Zoom to fit" icon_viewmagfit in
  let _ = GMenu.menu_item ~packing:menuvue#add () in
  let itemShowGrid =  GMenu.check_menu_item ~label:(i18n "Show Grid") ~show_toggle:true ~packing:menuvue#add () in

(*  let itemZoomSelection = itemVue "Adapter à la selection" in *)

  let _ = GMenu.menu_item ~packing:menuvue#add () in
  let itemPleinEcran = GMenu.check_menu_item ~label:(i18n "Full screen") ~show_toggle:true ~packing:menuvue#add () in


    (* menu Configuration *)
  let itemconfiguration =
    GMenu.menu_item ~label:(i18n "Configuration") ~packing:mbarprincipale#add () in
  let menuconfiguration = GMenu.menu ~packing:itemconfiguration#set_submenu () in
  let itemConfiguration s = GMenu.menu_item ~label:(i18n s) ~packing:menuconfiguration#add () in
  let itemUnites = GMenu.menu_item ~label:(i18n "Units") ~packing:menuconfiguration#add () in
  let menuUnites = GMenu.menu ~packing:itemUnites#set_submenu () in

  let itemUniteDistance = GMenu.menu_item ~label:(i18n "Distances") ~packing:menuUnites#add () in
  let menuDistance = GMenu.menu ~packing:itemUniteDistance#set_submenu () in  
  let itemCms =  GMenu.radio_menu_item ~label:(i18n "Centimeters") ~packing:menuDistance#add () in
  let itemDistance s =  GMenu.radio_menu_item ~group:itemCms#group ~label:(i18n s) ~packing:menuDistance#add () in
  let itemInchs =  itemDistance "Inchs" in
  let itemPixels =  itemDistance "Points" in

  let itemUniteAngle = GMenu.menu_item ~label:(i18n "Angles") ~packing:menuUnites#add () in
  let menuAngle = GMenu.menu ~packing:itemUniteAngle#set_submenu () in
  let itemDegres =  GMenu.radio_menu_item ~label:(i18n "Degres") ~packing:menuAngle#add () in
  let itemAngle s =  GMenu.radio_menu_item ~group:itemDegres#group ~label:(i18n s) ~packing:menuAngle#add () in
  let itemRadians =  itemAngle "Radians" in

 

  let itemCouleurFond = itemConfiguration "Background color" in
  let menuCouleurFond = GMenu.menu ~packing:itemCouleurFond#set_submenu () in
  let itemBlancFond = GMenu.radio_menu_item  ~label:(i18n "White") ~packing:menuCouleurFond#add () in
  let itemCoulFond s = GMenu.radio_menu_item ~group:itemBlancFond#group ~label:(i18n s) ~packing:menuCouleurFond#add () in
  let itemVertFond = itemCoulFond "Green" in
  let itemJauneFond = itemCoulFond "Yellow" in
  let itemBleuFond = itemCoulFond "Blue" in
  let itemGrisFond = itemCoulFond "Grey" in
  let itemRougeFond = itemCoulFond "Red" in
  let itemOrangeFond = itemCoulFond "Orange" in
  let itemVioletFond = itemCoulFond "Purple" in
  let itemRoseFond = itemCoulFond "Pink" in

  let itemAttributDefaut =  itemConfiguration "Default properties" in
  let menuAttributDefaut =  GMenu.menu ~packing:itemAttributDefaut#set_submenu () in
  let itemAttributDef s = GMenu.menu_item ~label:(i18n s) ~packing:menuAttributDefaut#add () in

  let itemCouleurDefaut = itemAttributDef "Default color" in
  let menuCouleurDefaut = GMenu.menu ~packing:itemCouleurDefaut#set_submenu () in
  let itemNoirDefaut = GMenu.radio_menu_item  ~label:(i18n "Black") ~packing:menuCouleurDefaut#add () in
  let itemCoulDefaut s = GMenu.radio_menu_item ~group:itemNoirDefaut#group ~label:(i18n s) ~packing:menuCouleurDefaut#add () in
  let itemVertDefaut = itemCoulDefaut "Green" in
  let itemJauneDefaut = itemCoulDefaut "Yellow" in
  let itemBleuDefaut = itemCoulDefaut "Blue" in
  let itemGrisDefaut = itemCoulDefaut "Grey" in
  let itemRougeDefaut = itemCoulDefaut "Red" in
  let itemOrangeDefaut = itemCoulDefaut "Orange" in
  let itemVioletDefaut = itemCoulDefaut "Purple" in
  let itemRoseDefaut = itemCoulDefaut "Pink" in

  let itemCouleurPointDefaut = itemAttributDef "Default point color" in
  let menuCouleurPointDefaut = GMenu.menu ~packing:itemCouleurPointDefaut#set_submenu () in
  let itemNoirPointDefaut = GMenu.radio_menu_item  ~label:(i18n "Black") ~packing:menuCouleurPointDefaut#add () in
  let itemCoulPointDefaut s = GMenu.radio_menu_item ~group:itemNoirPointDefaut#group ~label:(i18n s) ~packing:menuCouleurPointDefaut#add () in
  let itemVertPointDefaut = itemCoulPointDefaut "Green" in
  let itemJaunePointDefaut = itemCoulPointDefaut "Yellow" in
  let itemBleuPointDefaut = itemCoulPointDefaut "Blue" in
  let itemGrisPointDefaut = itemCoulPointDefaut "Grey" in
  let itemRougePointDefaut = itemCoulPointDefaut "Red" in
  let itemOrangePointDefaut = itemCoulPointDefaut "Orange" in
  let itemVioletPointDefaut = itemCoulPointDefaut "Purple" in
  let itemRosePointDefaut = itemCoulPointDefaut "Pink" in

  let itemLineStyleDefaut = itemAttributDef "Default line style" in
  let menuLineStyleDefaut = GMenu.menu ~packing:itemLineStyleDefaut#set_submenu () in
  let itemPleinDefaut = GMenu.radio_menu_item ~label:(i18n "Solid") ~packing:menuLineStyleDefaut#add () in
  let itemPointilleDefaut = GMenu.radio_menu_item 
      ~group:itemPleinDefaut#group
      ~label:(i18n  "Dash") 
      ~packing:menuLineStyleDefaut#add ()  in

  let itemPointStyleDefaut = itemAttributDef "Default point style" in
  let menuPointStyleDefaut = GMenu.menu ~packing:itemPointStyleDefaut#set_submenu () in
  let itemCroixDefaut = GMenu.radio_menu_item ~label:(i18n "Cross") ~packing:menuPointStyleDefaut#add () in
  let itemformeDefaut s = GMenu.radio_menu_item 
      ~group:itemPleinDefaut#group
      ~label:(i18n  s) 
      ~packing:menuPointStyleDefaut#add ()  in
  
  let itemRondDefaut = itemformeDefaut "Circle" in 
  let itemCarreDefaut = itemformeDefaut "Square" in 
  let itemRondPleinDefaut = itemformeDefaut "Filled circle" in 
  let itemCarrePleinDefaut = itemformeDefaut "Filled square" in 

  let itemEpaisseurDefaut = itemAttributDef "Default width" in
  let menuEpaisseurDefaut = GMenu.menu ~packing:itemEpaisseurDefaut#set_submenu () in
  let itemEpaisseur2Defaut = GMenu.radio_menu_item ~label:(i18n "Normal") ~packing:menuEpaisseurDefaut#add () in

  let itemEpaissDefaut s = GMenu.radio_menu_item 
      ~group:itemEpaisseur2Defaut#group 
      ~label:(i18n s) 
      ~packing:menuEpaisseurDefaut#add () in
  let itemEpaisseur1Defaut = itemEpaissDefaut "Fine" in
  let itemEpaisseur3Defaut = itemEpaissDefaut "Thick" in
  let itemEpaisseur4Defaut = itemEpaissDefaut "Very thick" in

  let itemCoucheDefaut = itemAttributDef "Default layer" in
  let menuCoucheDefaut = GMenu.menu ~packing:itemCoucheDefaut#set_submenu () in

(* On crée d'abord que le premier pour avoir son groupe et ensuite les autres donc à partir de 1 pas de 0 *)

  let itemCoucheDefaut = Array.make !!nombre_de_couches 
    (
      let item = GMenu.radio_menu_item ~packing:menuCoucheDefaut#add () in 
	item#add (itemLayersLabelsDefaut.(0)#coerce);
	item
    ) 
  in
  let creeItemCoucheDefaut i = 
    let item = GMenu.radio_menu_item 
      ~group:(itemCoucheDefaut.(0))#group 
      ~packing:menuCoucheDefaut#add () in
      item#add (itemLayersLabelsDefaut.(i)#coerce);
      item
  in
  let _ = for i=1 to (!!nombre_de_couches-1) do
    itemCoucheDefaut.(i) <- (creeItemCoucheDefaut i)
  done
 in


  let itemBarEtat =  GMenu.check_menu_item           
      ~label:(i18n "Hide status bar")
      ~packing:menuconfiguration#add ()in 

  let itemBarresOutils = itemConfiguration "Toolbars" in
  let menuBarresOutils =  GMenu.menu ~packing:itemBarresOutils#set_submenu () in
  let itembarresoutils s = GMenu.check_menu_item ~label:(i18n s) ~packing:menuBarresOutils#add () in
  let itemBarrePrincipale = itembarresoutils "Hide main toolbar" in
  let itemBarreVue =  itembarresoutils "Hide view toolbar" in
  let itemBarrePoints =  itembarresoutils "Hide points toolbar" in
  let itemBarreDroites =  itembarresoutils "Hide lines toolbar" in
  let itemBarreCercles =  itembarresoutils "Hide circles toolbar" in
  let itemBarreTransformations =  itembarresoutils "Hide transformations toolbar" in
  let itemBarreFacts =  itembarresoutils "Hide facts toolbar" in
  let itemBarreMeasures =  itembarresoutils "Hide measures toolbar" in
  let itemBarreLabels =  itembarresoutils "Hide labels toolbar" in

    (* menu Aide *)
  let itemaide =
    GMenu.menu_item 
      ~label:(i18n "Help") 
      ~packing:mbarprincipale#add () 
      ~right_justified:true 
  in
  let menuaide = GMenu.menu ~packing:itemaide#set_submenu () in

  let _ = GMenu.menu_item ~packing:menuaide#add () in
  let itemApropos = GMenu.menu_item ~label:(i18n "About") ~packing:menuaide#add () in

  let notebookdocs = GPack.notebook ~packing:(vbox#pack ~expand:true) () in

  let document_guis = ref [] in

  let add_document_tab nb () =
    notebookdocs#set_show_tabs true;
    document_guis:= (Document_gui.create_document_tab notebookdocs nb ())::!document_guis
  in
  
  let _ = add_document_tab 0 () in
  let _ = notebookdocs#set_show_tabs true in (* TODO temp *)
    
  let remove_document_tab () =
    notebookdocs#remove_page !Document_managment.current_document_nb;
    if false then (* TODO check number of tabs, need an new fonction in lablgtk2 *)
      notebookdocs#set_show_tabs false 
  in
  

  (* Text input *)

  let expander_text_input = GBin.expander ~label:(i18n "Input in natural language") ~packing:vbox#pack () in
  (* let _ = expander_text_input#set_expanded true in *)
  let hbox_text_input = GPack.hbox  ~packing:expander_text_input#add () in
  let help_text_input = GButton.button ~label:"Help" ~packing:(hbox_text_input#pack) () in
    
  let text_input = GEdit.entry ~packing:(hbox_text_input#pack ~expand:true) () in
  let command_list = GEdit.combo_box_text 
    ~strings:["todo";"titi";"toto"]  
    ~packing:(hbox_text_input#pack) ()in

  (* Barre d'état *)

  let statusbar = GMisc.statusbar  ~packing:(vbox#pack ~expand:false)  () in

  object
    val window = window
    method document_gui =
      try 
	List.nth !document_guis !Document_managment.current_document_nb
      with
	_ -> failwith ("this gui does not exist"^(string_of_int !Document_managment.current_document_nb))

    val statusbar = statusbar
		    
    val itemQuitter = itemQuitter
		     
    val itempointclique = itempointclique
    val itempointcoords = itempointcoords
    val itempointmilieu = itempointmilieu
    val itempointbetween = itempointbetween
    val itempointleftturn = itempointleftturn
    val itemintersectiondroites = itemintersectiondroites
    val itemintersectioncercledroite = itemintersectioncercledroite
    val itemintersectioncercles = itemintersectioncercles
    val itemdroitedeuxpoints = itemdroitedeuxpoints
    val itemhalfline = itemhalfline
    val itemdroitemediatrice = itemdroitemediatrice
    val itemdroitebissectrice = itemdroitebissectrice
    val itemsegment = itemsegment
    val itemvecteur = itemvecteur
    val itemdroiteparallele = itemdroiteparallele
    val itemdroiteorthogonale = itemdroiteorthogonale
    val itemcerclecentrepoint = itemcerclecentrepoint
    val itemcercletroispoints = itemcercletroispoints       
    val itemcerclediametre = itemcerclediametre
			       
    val itemtranslation = itemtranslation
    val itemsymetriepoint = itemsymetriepoint
    val itemsymetriedroite = itemsymetriedroite

    val itemtestalignement = itemtestalignement
    val itemtestpointsconfondus = itemtestpointsconfondus
    val itemtestparallelisme = itemtestparallelisme
    val itemtestperpendicularite = itemtestperpendicularite
    val itemtestbetween = itemtestbetween
    val itemtestleftturn = itemtestleftturn

    val itemmesureangle = itemmesureangle
    val itemmesuredistance = itemmesuredistance
    val itemmesurearea = itemmesurearea


    val itemfreelabel = itemfreelabel

    val itemdeplacepoint = itemdeplacepoint

    val itemgommeobjet = itemgommeobjet

    val itemLayersLabelsSelection = itemLayersLabelsSelection
    val itemLayersLabelsDefaut = itemLayersLabelsDefaut
    val itemLayersLabels = itemLayersLabels
    
    method itemFormalLanguage = itemFormalLanguage
    method itemNarboux = itemNarboux
    method itemGuilhot = itemGuilhot

    method itemLayersLabelsSelection = itemLayersLabelsSelection
    method itemLayersLabelsDefaut = itemLayersLabelsDefaut
    method itemLayersLabels = itemLayersLabels
			   
    method window = window
    method notebookdocs = notebookdocs
    method statusbar = statusbar	

    method itemBarEtat = itemBarEtat

    method barreoutilsPrinc = handle_boxPrinc
    method barreoutilsVue = handle_boxVue
    method barreoutilsPoints = handle_boxPoints
    method barreoutilsDroites = handle_boxDroites
    method barreoutilsCercles = handle_boxCercles
    method barreoutilsTransformations = handle_boxTransformations
    method barreoutilsFacts = handle_boxTests
    method barreoutilsMeasures = handle_boxMeasures
    method barreoutilsLabels = handle_boxLabels

    method itemNouveau = itemNouveau
    method itemSaveAs = itemSaveAs
    method itemSave = itemSave
    method itemOuvrir = itemOuvrir
    method itemBitmap = itemBitmap
    method itemSvg = itemSvg
    method itemKig = itemKig
    method itemCar = itemCar
    method itemPs = itemPs
    method itemEukleides = itemEukleides
    method itemPstEucl = itemPstEucl

    method itemQuitter = itemQuitter
    method items_recent_files = items_recent_files
    method items_recent_files_labels = items_recent_files_labels

    method itemDefaire = itemDefaire
    method itemRefaire = itemRefaire

    method itemDeleteTraces = itemDeleteTraces

    method itemSelectionnerTout = itemSelectionnerTout 	  
    method itemSelectionnerToutVisible = itemSelectionnerToutVisible 	
    method itemSelectionnerToutCache = itemSelectionnerToutCache 	
    method itemSelectionDroites = itemSelectionDroites
    method itemSelectionSegments = itemSelectionSegments
    method itemSelectionPoints = itemSelectionPoints
    method itemSelectionCercles = itemSelectionCercles
    method itemSelectionVecteurs = itemSelectionVecteurs
    method itemSelectionLibres = itemSelectionLibres
    method itemSelectionAucune = itemSelectionAucune
    method itemSelectionInverser = itemSelectionInverser 
    method itemSelectionObjet = itemSelectionObjet 
    method itemToutRendreVisible = itemToutRendreVisible 
    method itemSupprimerSelection = itemSupprimerSelection

    method itemApropos= itemApropos

    method itempointclique = itempointclique
    method itempointcoords = itempointcoords
    method itempointmilieu = itempointmilieu
    method itempointbetween = itempointbetween
    method itempointleftturn = itempointleftturn

    method itempointCercle = itempointCercle
    method itempointDroite = itempointDroite

    method itemintersectiondroites = itemintersectiondroites
    method itemintersectioncercledroite = itemintersectioncercledroite
    method itemintersectioncercles = itemintersectioncercles
				
    method itemdroitedeuxpoints = itemdroitedeuxpoints
    method itemhalfline = itemhalfline
    method itemdroitemediatrice = itemdroitemediatrice
    method itemdroitebissectrice = itemdroitebissectrice
    method itemsegment = itemsegment
    method itemvecteur = itemvecteur
    method itemdroiteparallele = itemdroiteparallele
    method itemdroiteorthogonale = itemdroiteorthogonale
    method itemcerclecentrepoint = itemcerclecentrepoint
    method itemcercletroispoints = itemcercletroispoints
    method itemcerclediametre = itemcerclediametre
				  
    method itemtranslation = itemtranslation
    method itemsymetriepoint = itemsymetriepoint
    method itemsymetriedroite = itemsymetriedroite
 
    method itemfreelabel = itemfreelabel
    method itemlabelpoint = itemlabelpoint

    method itemtestalignement = itemtestalignement
    method itemtestpointsconfondus = itemtestpointsconfondus
    method itemtestparallelisme = itemtestparallelisme
    method itemtestperpendicularite = itemtestperpendicularite
    method itemtestbetween = itemtestbetween
    method itemtestleftturn = itemtestleftturn

    method itemmesureangle = itemmesureangle
    method itemmesuredistance = itemmesuredistance
    method itemmesurearea = itemmesurearea

    method itemdeplacepoint = itemdeplacepoint
    method itemATP = itemATP
    method itemCoq = itemCoq
    method itemCoqIde = itemCoqIde
    method itemCoqInteractive = itemCoqInteractive

    method itemRouge = itemRouge
    method itemBleu = itemBleu
    method itemVert = itemVert
    method itemJaune = itemJaune
    method itemGris = itemGris
    method itemNoir = itemNoir
    method itemOrange = itemOrange
    method itemViolet = itemViolet
    method itemRose = itemRose

    method itemPlein = itemPlein
    method itemPointille = itemPointille

    method itemEpaisseur1 = itemEpaisseur1
    method itemEpaisseur2 = itemEpaisseur2 
    method itemEpaisseur3 = itemEpaisseur3
    method itemEpaisseur4 = itemEpaisseur4

    method itemCoucheSelection = itemCoucheSelection
    method itemCouches = itemCouches

    method itemCache = itemCache
       
    method itemgommeobjet = itemgommeobjet
    method itemPleinEcran = itemPleinEcran
    method itemZoomAvant = itemZoomAvant
    method itemZoomArriere = itemZoomArriere
    method itemZoomAjuste = itemZoomAjuste
    method itemShowGrid = itemShowGrid

    method itemRougeFond = itemRougeFond
    method itemBleuFond = itemBleuFond
    method itemVertFond = itemVertFond
    method itemJauneFond = itemJauneFond
    method itemGrisFond = itemGrisFond
    method itemBlancFond = itemBlancFond
    method itemOrangeFond = itemOrangeFond
    method itemVioletFond = itemVioletFond
    method itemRoseFond = itemRoseFond

    method itemRougeDefaut = itemRougeDefaut
    method itemBleuDefaut = itemBleuDefaut
    method itemVertDefaut = itemVertDefaut
    method itemJauneDefaut = itemJauneDefaut
    method itemGrisDefaut = itemGrisDefaut
    method itemNoirDefaut = itemNoirDefaut
    method itemOrangeDefaut = itemOrangeDefaut
    method itemVioletDefaut = itemVioletDefaut
    method itemRoseDefaut = itemRoseDefaut
 
    method itemRougePointDefaut = itemRougePointDefaut
    method itemBleuPointDefaut = itemBleuPointDefaut
    method itemVertPointDefaut = itemVertPointDefaut
    method itemJaunePointDefaut = itemJaunePointDefaut
    method itemGrisPointDefaut = itemGrisPointDefaut
    method itemNoirPointDefaut = itemNoirPointDefaut
    method itemOrangePointDefaut = itemOrangePointDefaut
    method itemVioletPointDefaut = itemVioletPointDefaut
    method itemRosePointDefaut = itemRosePointDefaut

    method itemPleinDefaut = itemPleinDefaut
    method itemPointilleDefaut = itemPointilleDefaut

    method itemCroixDefaut = itemCroixDefaut
    method itemRondDefaut = itemRondDefaut
    method itemCarreDefaut = itemCarreDefaut
    method itemRondPleinDefaut = itemRondPleinDefaut
    method itemCarrePleinDefaut = itemCarrePleinDefaut
	

    method itemEpaisseur1Defaut = itemEpaisseur1Defaut
    method itemEpaisseur2Defaut = itemEpaisseur2Defaut
    method itemEpaisseur3Defaut = itemEpaisseur3Defaut
    method itemEpaisseur4Defaut = itemEpaisseur4Defaut

    method itemCoucheDefaut = itemCoucheDefaut

    method itemUnitéCms = itemCms
    method itemUnitéPouces = itemInchs
    method itemUnitéPoints = itemPixels

    method itemUnitéDegres = itemDegres
    method itemUnitéRadians = itemRadians

    method itemBarrePrincipale = itemBarrePrincipale 
    method itemBarreVue =  itemBarreVue
    method itemBarrePoints = itemBarrePoints
    method itemBarreDroites =itemBarreDroites
    method itemBarreCercles =itemBarreCercles 
    method itemBarreTransformations =itemBarreTransformations
    method itemBarreFacts =itemBarreFacts
    method itemBarreMeasures =itemBarreMeasures
    method itemBarreLabels =itemBarreLabels

    method boutonPointLibre =  boutonPointLibre
    method boutonPointCoords =  boutonPointCoords      
    method boutonPointDroite = boutonPointDroite
    method boutonPointCercle = boutonPointCercle
    method boutonPointMilieu = boutonPointMilieu
    method boutonPointInterDroites = boutonPointInterDroites
    method boutonPointInterCercleDroite = boutonPointInterCercleDroite
    method boutonPointInterCercles = boutonPointInterCercles
    method boutonPointLeftTurn = boutonPointLeftTurn
    method boutonPointBetween = boutonPointBetween

    method boutonDroiteDeuxPoints = boutonDroiteDeuxPoints
    method boutonDemieDroite = boutonDemieDroite
    method boutonDroiteMediatrice = boutonDroiteMediatrice
    method boutonDroiteBissectrice = boutonDroiteBissectrice
    method boutonDroiteVecteur = boutonDroiteVecteur
    method boutonDroiteSegment = boutonDroiteSegment
    method boutonDroiteParallele = boutonDroiteParallele
    method boutonDroiteOrthogonale = boutonDroiteOrthogonale

    method boutonCercleCentrePoint = boutonCercleCentrePoint
    method boutonCercleTroisPoints = boutonCercleTroisPoints
    method boutonCercleDiametre = boutonCercleDiametre

    method boutonSymetriePoint = boutonSymetriePoint
    method boutonSymetrieDroite = boutonSymetrieDroite
    method boutonTranslation = boutonTranslation
   
    method boutonTestAlignement = boutonTestAlignement
    method boutonTestPointsConfondus = boutonTestPointsConfondus
    method boutonTestParallelisme = boutonTestParallelisme
    method boutonTestPerpendicularite = boutonTestPerpendicularite
    method boutonTestCongruentSegments = boutonTestCongruentSegments
    method boutonTestCongruentAngles = boutonTestCongruentAngles
    method boutonTestBetween = boutonTestBetween
    method boutonTestLeftTurn = boutonTestLeftTurn

(*  method boutonATP = boutonATP
    method boutonCoq = boutonCoq
*)
    method boutonMesureAngle = boutonMesureAngle
    method boutonMesureDistance = boutonMesureDistance
    method boutonMesureArea = boutonMesureArea

    method boutonTexteClique = boutonTexteClique
    method boutonTextePoint = boutonTextePoint

    method boutonPleinEcran = boutonPleinEcran
    method boutonRepere = boutonRepere
    method boutonZoomAvant = boutonZoomAvant
    method boutonZoomArriere = boutonZoomArriere
    method boutonZoomAjuste = boutonZoomAjuste
    method boutonMovePaper = boutonMovePaper

    method boutonNouveau = boutonNouveau
    method boutonOuvrir = boutonOuvrir
    method boutonSave = boutonSave
    method boutonSaveAs = boutonSaveAs

    method boutonDeplacer = boutonDeplacer
    method boutonSelectionnerObjet = boutonSelectionnerObjet  

    method boutonRefaire = boutonRefaire
    method boutonDefaire = boutonDefaire

    method boutonInterrompre = boutonInterrompre
    method boutonSupprimer =  boutonSupprimer
    method utiliser_pointeur_deplacer = utiliser_pointeur_deplacer
    method utiliser_pointeur_creer = utiliser_pointeur_creer
    method utiliser_pointeur_selectionner = utiliser_pointeur_selectionner 
    method utiliser_pointeur_effacer = utiliser_pointeur_effacer
    method utiliser_pointeur_move_paper = utiliser_pointeur_move_paper

    method add_document_tab = add_document_tab
    method remove_document_tab = remove_document_tab
  end

    
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
  
