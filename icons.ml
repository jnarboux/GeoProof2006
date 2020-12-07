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

(* Names of the icons *)

let icon_anglebisector = "angle-bisector"
let icon_rooster = "rooster"
let icon_lightbulb = "lightbulb"
let icon_filesaveas = "filesaveas"
let icon_perpendicular = "perpendicular"
let icon_testcollinear = "test-collinear"
let icon_angle = "angle"
let icon_filesave = "filesave"
let icon_pointcircle = "pointcircle"
let icon_testeqpoints = "test-eq-points"
let icon_area = "area"
let icon_grid = "grid"
let icon_pointline = "pointline"
let icon_testleftturn = "test-left-turn"
let icon_between = "between"
let icon_intersectionCircles = "intersectionCircles"
let icon_point = "point"
let icon_point_coords = "point_coords"
let icon_testparallel = "test-parallel"
let icon_centralsymmetry = "centralsymmetry"
let icon_intersection = "intersection"
let icon_preferences = "preferences"
let icon_testperpendicular = "test-perpendicular"
let icon_circlebcp = "circlebcp"
let icon_labelpoint = "label-point"
let icon_redo = "redo"
let icon_translation = "translation"
let icon_circlebtp = "circlebtp"
let icon_label = "label"
let icon_revert = "revert"
let icon_trash = "trash"
let icon_circlediam = "circlediam"
let icon_launch = "launch"
let icon_save_as = "save_as"
let icon_undo = "undo"
let icon_circlelineintersection = "circlelineintersection"
let icon_left_turn = "left_turn"
let icon_save = "save"
let icon_vector = "vector"
let icon_congruent_angles = "congruent_angles"
let icon_line = "line"
let icon_half_line = "half-line"
let icon_segment = "segment"
let icon_viewmag1 = "viewmag1"
let icon_congruent_segments = "congruent_segments"
let icon_mediatrice = "mediatrice"
let icon_select = "select"
let icon_viewmagfit = "viewmagfit"
let icon_distance = "distance"
let icon_midpoint = "midpoint"
let icon_stock_close = "stock_close"
let icon_viewmagminus = "viewmag-"
let icon_geoproof = "geoproof"
let icon_mirrorpoint = "mirrorpoint"
let icon_stock_directory_open = "stock_directory_open"
let icon_viewmagplus = "viewmag+"
let icon_edit = "edit"
let icon_move = "move"
let icon_stock_ok = "stock"
let icon_window_fullscreen = "window_fullscreen"
let icon_eraser = "eraser"
let icon_new = "new"
let icon_stock_stop = "stock_stop"
let icon_filenew = "filenew"
let icon_open = "open"
let icon_stop = "stop"
let icon_fileopen = "fileopen"
let icon_parallel = "parallel"
let icon_testbetween = "test-between"
let icon_apropos = "apropos"
let icon_quit = "quit"
let icon_pointing_finger = "pointing_finger"

let table = [
icon_anglebisector,Anglebisector_svg.t;
icon_lightbulb,Lightbulb_svg.t;
icon_rooster,Rooster_svg.t;
icon_filesaveas,Filesaveas_svg.t;
icon_perpendicular,Perpendicular_svg.t;
icon_testcollinear,Testcollinear_svg.t;
icon_angle,Angle_svg.t;
icon_filesave,Filesave_svg.t;
icon_pointcircle,Pointcircle_svg.t;
icon_testeqpoints,Testeqpoints_svg.t;
icon_area,Area_svg.t;
icon_grid,Grid_svg.t;
icon_pointline,Pointline_svg.t;
icon_testleftturn,Testleftturn_svg.t;
icon_between,Between_svg.t;
icon_intersectionCircles,IntersectionCircles_svg.t;
icon_point,Point_svg.t;
icon_point_coords,Point_coords_svg.t;
icon_testparallel,Testparallel_svg.t;
icon_centralsymmetry,Centralsymmetry_svg.t;
icon_intersection,Intersection_svg.t;
icon_preferences,Preferences_svg.t;
icon_testperpendicular,Testperpendicular_svg.t;
icon_circlebcp,Circlebcp_svg.t;
icon_labelpoint,Labelpoint_svg.t;
icon_redo,Redo_svg.t;
icon_translation,Translation_svg.t;
icon_circlebtp,Circlebtp_svg.t;
icon_label,Label_svg.t;
icon_revert,Revert_svg.t;
icon_trash,Trash_svg.t;
icon_circlediam,Circlediam_svg.t;
icon_launch,Launch_svg.t;
icon_save_as,Save_as_svg.t;
icon_undo,Undo_svg.t;
icon_circlelineintersection,Circlelineintersection_svg.t;
icon_left_turn,Left_turn_svg.t;
icon_save,Save_svg.t;
icon_vector,Vector_svg.t;
icon_congruent_angles,Congruent_angles_svg.t;
icon_line,Line_svg.t;
icon_half_line,Half_line_svg.t;
icon_segment,Segment_svg.t;
icon_viewmag1,Viewmag1_svg.t;
icon_congruent_segments,Congruent_segments_svg.t;
icon_mediatrice,Mediatrice_svg.t;
icon_select,Select_svg.t;
icon_viewmagfit,Viewmagfit_svg.t;
icon_distance,Distance_svg.t;
icon_midpoint,Midpoint_svg.t;
icon_stock_close,Stock_close_svg.t;
icon_viewmagminus,Viewmagminus_svg.t;
icon_geoproof,Geoproof_svg.t;
icon_mirrorpoint,Mirrorpoint_svg.t;
icon_stock_directory_open,Stock_directory_open_svg.t;
icon_viewmagplus,Viewmagplus_svg.t;
icon_edit,Edit_svg.t;
icon_move,Move_svg.t;
icon_stock_ok,Stock_ok_svg.t;
icon_window_fullscreen,Window_fullscreen_svg.t;
icon_eraser,Eraser_svg.t;
icon_new,New_svg.t;
icon_stock_stop,Stock_stop_svg.t;
icon_filenew,Filenew_svg.t;
icon_open,Open_svg.t;
icon_stop,Stop_svg.t;
icon_fileopen,Fileopen_svg.t;
icon_parallel,Parallel_svg.t;
icon_testbetween,Testbetween_svg.t;
icon_apropos,Apropos_svg.t;
icon_quit,Quit_svg.t;
icon_pointing_finger,Pointing_finger_svg.t
]


(* Render a svg iconed contained in a string to a pixbuf *)

let get_pixbuf icon_name sizex sizey = 
   let svg = try List.assoc icon_name table
     with _ -> Debug.pdb icon_name; List.assoc icon_stock_stop table
   in
   let size_cb = Rsvg.at_max_size sizex sizey in
   let pixbuf = Rsvg.render_from_string ~size_cb svg in
   pixbuf	

let get_icon icon_name pixel_size =
    let pixbuf = get_pixbuf icon_name pixel_size pixel_size in
    let image = GMisc.image ~pixbuf () in	
    fun () -> image#coerce
