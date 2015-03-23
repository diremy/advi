(*#load "graphics.cma";;
#load "unix.cma";;*)
open Agraphics;;
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* titi5.ml: a game writtent by Val�rie M�nissier-Morain to be included in a
book as encapsulated PostScript.

   compile it as a standalone using the graphps version of the graphics
   library. For instance

   ocamlc -I .. graphics.cmo titi5.ml

   produces a.out. Then run ./a.out to write the PostScript file
   "titi5.eps", than can be visualized with a PostScript interpreter
   (gs, ghostview, gv, ...).
*)

open Graphics;;

open_graph " titi5";;

let rec interval i j =
  (* Cas d'arr�t de la r�cursivit� *)
  if i = j then [i] else
  if i < j then i :: interval (i + 1) j
  else i :: interval (i - 1) j;;

let interval_sans i j inf sup =
  if i = inf then interval (i + 1) sup else
  if i = sup then interval inf (i - 1) else
  if i = j then interval (i + 1) sup @ interval inf (i - 1) else
  if i < j then interval (i + 1) sup @ interval inf (i - 1)
  else interval (i - 1) inf @ interval (i + 1) sup;;

type case = Vide | Occup�e of int;;

type state0 = {
  mutable s�lection : (int * int) option;
  mutable niveau : int;
};;

type game_zone = {
  tableau : case array array;
  mutable colonnes : int;
  mutable lignes : int;
  mutable nb_colonnes : int;
  mutable nb_lignes : int;
  mutable d�butc : int;
  mutable finc : int;
  mutable d�butl : int;
  mutable finl : int;
};;

type information = {
  pi�ce1 : int * int;
  pi�ce2 : int * int;
  couleur : int;
};;

type state = {
  mutable s�lection : (int * int) option;
  mutable niveau : int;
  mutable zone_de_jeu : game_zone;
  mutable temps_r�f�rence : float;
  mutable historique : information list;
  mutable score : int;
};;

let colonnes_of_niveau niveau = 4 + 2 * (niveau / 2);;

let lignes_of_niveau niveau = 2 + niveau + (niveau mod 2);;

let nb_couleurs_of_niveau niveau = 4 + 2 * niveau;;

let bord_gauche = 1
and bord_droit = 1
and bord_bas = 1
and bord_haut = 1;;

let �tat = {
  s�lection = None;
  niveau = 1;
  zone_de_jeu =
  { colonnes = 0;
    lignes = 0;
    d�butc = 0;
    finc = 0;
    d�butl = 0;
    finl = 0;
    nb_colonnes = 0;
    nb_lignes = 0;
    tableau = Array.make_matrix 0 0 Vide };
  temps_r�f�rence = 0.0;
  historique = [];
  score = 0;
};;

let cr�ation_tableau_ordonn� () =
  for j = �tat.zone_de_jeu.d�butl to �tat.zone_de_jeu.finl do
    let i = ref �tat.zone_de_jeu.d�butc in
    while !i <= �tat.zone_de_jeu.finc do
       �tat.zone_de_jeu.tableau.(!i).(j) <-
         Occup�e (Random.int (nb_couleurs_of_niveau �tat.niveau));
       �tat.zone_de_jeu.tableau.(!i + 1).(j) <-
         �tat.zone_de_jeu.tableau.(!i).(j);
       i := !i + 2;
    done;
  done;;

let m�lange_couleurs_tableau () =
  for i = �tat.zone_de_jeu.d�butc to �tat.zone_de_jeu.finc do
    for j = �tat.zone_de_jeu.d�butl to �tat.zone_de_jeu.finl do
      let c = i + Random.int (�tat.zone_de_jeu.finc - i + 1)
      and l = j + Random.int (�tat.zone_de_jeu.finl - j + 1) in
      let tmp = �tat.zone_de_jeu.tableau.(c).(l) in
      �tat.zone_de_jeu.tableau.(c).(l) <- �tat.zone_de_jeu.tableau.(i).(j);
      �tat.zone_de_jeu.tableau.(i).(j) <- tmp
    done;
  done;;

let nouvelle_zone_de_jeu () =
    let col = colonnes_of_niveau �tat.niveau
    and lig = lignes_of_niveau �tat.niveau in
    let ncol = bord_gauche + col + bord_droit
    and nlig = bord_bas + lig + bord_haut in
  �tat.zone_de_jeu <-
  { colonnes = col;
    lignes = lig;
    d�butc = bord_gauche;
    finc = bord_gauche + col - 1;
    d�butl = bord_bas;
    finl = bord_bas + lig - 1;
    nb_colonnes = ncol;
    nb_lignes = nlig;
    tableau = Array.make_matrix ncol nlig Vide; };
  cr�ation_tableau_ordonn� ();
  m�lange_couleurs_tableau ();;

let case_vide (c, l) =
  �tat.zone_de_jeu.tableau.(c).(l) = Vide;;

let cases_jointes_segment_vertical (c1, l1) (c2, l2) =
  c1 = c2 &&
  let (minl, maxl) =
    if l1 < l2 then (l1, l2) else (l2, l1) in
       (* il n'y a pas de case interm�diaire *)
       maxl <= minl + 1
    || (* toutes les cases interm�diaires sont vides *)
       (List.for_all case_vide
         (List.map (function l -> (c1, l))
           (interval (minl + 1) (maxl - 1))));;

let cases_jointes_segment_horizontal (c1, l1) (c2, l2) =
  l1 = l2 &&
  let (minc, maxc) =
    if c1 < c2 then (c1, c2) else (c2, c1) in
       (* il n'y a pas de case interm�diaire *)
       maxc <= minc + 1
    || (* toutes les cases interm�diaires sont vides *)
         (List.for_all case_vide
         (List.map (function c -> (c, l1))
           (interval (minc + 1) (maxc - 1))));;

let cases_jointes_segment_hv (c1, l1) (c2, l2) =
   cases_jointes_segment_vertical (c1, l1) (c2, l2)
|| cases_jointes_segment_horizontal (c1, l1) (c2, l2);;

let rec cases_jointes_circuit_n_angles (c1, l1) (c2, l2) n =
  if n = 0 (* Cas de base de la r�cursivit� *)
  then cases_jointes_segment_hv (c1, l1) (c2, l2)
  else
    List.exists
      (function l ->
           case_vide (c1, l)
        && cases_jointes_segment_vertical (c1, l1) (c1, l)
        && cases_jointes_circuit_n_angles (c1, l) (c2, l2) (n - 1))
      (interval_sans l1 l2 0 (�tat.zone_de_jeu.nb_lignes - 1))
  || List.exists
      (function c ->
           case_vide (c, l1)
        && cases_jointes_segment_horizontal (c1, l1) (c, l1)
        && cases_jointes_circuit_n_angles (c, l1) (c2, l2) (n - 1))
         (interval_sans c1 c2 0 (�tat.zone_de_jeu.nb_colonnes - 1));;

let rec cases_jointes_circuit_au_plus_n_angles (c1, l1) (c2, l2) n =
  if n = 0
  then cases_jointes_segment_hv (c1, l1) (c2, l2)
  else
      cases_jointes_circuit_au_plus_n_angles (c1, l1) (c2, l2) (n - 1)
   || cases_jointes_circuit_n_angles (c1, l1) (c2, l2) n;;

let circuit_segment_vertical (c1, l1) (c2, l2) =
  if cases_jointes_segment_vertical (c1, l1) (c2, l2)
  then
    if c1 <> c2 || l1 <> l2
    then [(c1, l1); (c2, l2)]
    else [(c1, l1)]
  else raise Not_found;;

let circuit_segment_horizontal (c1, l1) (c2, l2) =
  if cases_jointes_segment_horizontal (c1, l1) (c2, l2) then
    if c1 <> c2 || l1 <> l2 then [(c1, l1); (c2, l2)] else [(c1, l1)]
  else raise Not_found;;

let circuit_segment (c1, l1) (c2, l2) =
  if cases_jointes_segment_vertical (c1, l1) (c2, l2)
  || cases_jointes_segment_horizontal (c1, l1) (c2, l2) then
    if c1 <> c2 || l1 <> l2 then [(c1, l1);(c2, l2)] else [(c1, l1)]
  else raise Not_found;;

let rec circuit_n_angles (c1, l1) (c2, l2) n =
  if n = 0 (* Cas de base de la r�cursivit� *)
  then circuit_segment (c1, l1) (c2, l2)
  else
    try
      let ligne =
        List.find
        (function l ->
           case_vide (c1, l) &&
           cases_jointes_segment_vertical (c1, l1) (c1, l) &&
           cases_jointes_circuit_n_angles (c1, l) (c2, l2) (n - 1))
      (interval_sans l1 l2 0 (�tat.zone_de_jeu.nb_lignes - 1)) in
      (c1, l1) :: circuit_n_angles (c1, ligne) (c2, l2) (n - 1)
    with Not_found ->
      let colonne =
        List.find
         (function c ->
             case_vide (c, l1) &&
             cases_jointes_segment_horizontal (c1, l1) (c, l1)
          && cases_jointes_circuit_n_angles (c, l1) (c2, l2) (n - 1))
          (interval_sans c1 c2 0 (�tat.zone_de_jeu.nb_colonnes - 1)) in
     (c1, l1) :: circuit_n_angles (colonne, l1) (c2, l2) (n - 1);;

let rec circuit_au_plus_n_angles (c1, l1) (c2, l2) n =
  if n = 0
  then circuit_segment (c1, l1) (c2, l2)
  else
    try circuit_au_plus_n_angles (c1, l1) (c2, l2) (n - 1)
    with Not_found -> circuit_n_angles (c1, l1) (c2, l2) n;;

let circuit_valide (c1, l1) (c2, l2) =
  match �tat.zone_de_jeu.tableau.(c1).(l1),
        �tat.zone_de_jeu.tableau.(c2).(l2) with
  | Vide, _ | _, Vide -> []
  | Occup�e coul1, Occup�e coul2 ->
      if coul1 = coul2
      then
        try
          circuit_au_plus_n_angles (c1, l1) (c2, l2) 2
        with Not_found -> []
      else [];;

let taille_pi�ce = 30;;

let grey = rgb 175 175 175;;

let dessiner_pi�ce0 (x, y) fond =
  if fond
  then set_color white
  else set_color grey;
  fill_rect x y taille_pi�ce taille_pi�ce;
  set_color foreground;
  draw_rect x y taille_pi�ce taille_pi�ce;;

let dessiner_rectangle_bord_arrondi x y w h arrondi =
  draw_poly [|
    (x, y + arrondi); (x, y + h - arrondi); (x + arrondi, y + h);
    (x + w - arrondi, y + h); (x + w, y + h - arrondi); (x + w, y + arrondi);
    (x + w - arrondi, y); (x + arrondi, y);
  |];;

let dessiner_pi�ce1 (x, y) fond =
  dessiner_pi�ce0 (x, y) fond;
  let bord = 5 (* �cartement du bord du carr� central *)
  and arrondi = 2 (* dimension du pan coup� *) in
    dessiner_rectangle_bord_arrondi
      (x + bord) (y + bord)
      (taille_pi�ce - 2 * bord) (taille_pi�ce - 2 * bord) arrondi;;

let dessiner_pi�ce2ou3 (x, y) fond orientation =
  dessiner_pi�ce0 (x, y) fond;
  let bord = 4 (* �cartement du bord du carr� central *)
  and espace = 6 (* espace entre les deux rectangles *)
  and arrondi = 1 in
  (* dimension du carr� interne dans lequel s'inscrivent
     les deux rectangles *)
  let dim_interne = taille_pi�ce - 2 * bord in
  let (w, h) = (* largeur et hauteur des deux rectangles *)
    if orientation
    then (dim_interne, (dim_interne - espace) / 2)
    else ((dim_interne - espace) / 2, dim_interne) in
  dessiner_rectangle_bord_arrondi
    (x + bord) (y + bord) w h arrondi;
  if orientation
  then dessiner_rectangle_bord_arrondi
         (x + bord) (y + bord + h + espace) w h arrondi
  else dessiner_rectangle_bord_arrondi
         (x + bord + w + espace) (y + bord) w h arrondi;;

let dessiner_pi�ce6 (x, y) fond =
  dessiner_pi�ce0 (x, y) fond;
  let bord = 5 (* �cartement du bord des carr�s centraux *)
  and espace = 6 (* espace entre les carr�s centraux *)
  and arrondi = 1 in
  (* c�t� des carr�s centraux *)
  let w = (taille_pi�ce - 2 * bord - espace) / 2 in
    dessiner_rectangle_bord_arrondi
      (x + bord) (y + bord) w w arrondi;
    dessiner_rectangle_bord_arrondi
      (x + bord + w + espace) (y + bord) w w arrondi;
    dessiner_rectangle_bord_arrondi
      (x + bord) (y + bord + w + espace) w w arrondi;
    dessiner_rectangle_bord_arrondi
      (x + bord + w + espace) (y + bord + w + espace) w w arrondi;;

let dessiner_pi�ce4ou5 (x, y) fond orientation =
  dessiner_pi�ce0 (x, y) fond;
  let bord = 5
  and espace = 4 in
  if orientation then begin
    draw_poly
      [|(x + bord, y + taille_pi�ce - bord);
        (x + taille_pi�ce - bord - espace, y + taille_pi�ce - bord);
        (x + bord, y + bord + espace)|];
    draw_poly
      [|(x + bord + espace, y + bord);
        (x + taille_pi�ce - bord, y + bord);
        (x + taille_pi�ce - bord, y + taille_pi�ce - bord - espace)|]
  end else begin
    draw_poly
      [|(x + bord + espace, y + taille_pi�ce - bord);
        (x + taille_pi�ce - bord, y + taille_pi�ce - bord);
        (x + taille_pi�ce - bord, y + bord + espace)|];
    draw_poly
      [|(x + bord, y + bord);
        (x + taille_pi�ce - bord - espace, y + bord);
        (x + bord, y + taille_pi�ce - bord - espace)|]
  end;;

let dessiner_pi�ce7 (x, y) fond =
  dessiner_pi�ce0 (x, y) fond;
  let bord = 4
  and espace = 4 in
  draw_poly
    [|(x + bord, y + bord + espace);
      (x + taille_pi�ce / 2 - espace, y + taille_pi�ce / 2);
      (x + bord, y + taille_pi�ce - bord - espace)|];
  draw_poly
    [|(x + taille_pi�ce - bord, y + bord + espace);
      (x + taille_pi�ce / 2 + espace, y + taille_pi�ce / 2);
      (x + taille_pi�ce - bord, y + taille_pi�ce - bord - espace)|];
  draw_poly
    [|(x + bord + espace, y + bord);
      (x + taille_pi�ce / 2, y + taille_pi�ce / 2 - espace);
      (x + taille_pi�ce - bord - espace, y + bord)|];
  draw_poly
    [|(x + bord + espace, y + taille_pi�ce - bord);
      (x + taille_pi�ce / 2, y + taille_pi�ce / 2 + espace);
      (x + taille_pi�ce - bord - espace, y + taille_pi�ce - bord)|];;

let dessiner_pi�ce (x, y) num�ro =
  match num�ro / 2 with
  | 0 -> dessiner_pi�ce0 (x, y) (num�ro mod 2 = 0)
  | 1 -> dessiner_pi�ce1 (x, y) (num�ro mod 2 = 0)
  | 2 -> dessiner_pi�ce2ou3 (x, y) (num�ro mod 2 = 0) true
  | 3 -> dessiner_pi�ce2ou3 (x, y) (num�ro mod 2 = 0) false
  | 4 -> dessiner_pi�ce4ou5 (x, y) (num�ro mod 2 = 0) true
  | 5 -> dessiner_pi�ce4ou5 (x, y) (num�ro mod 2 = 0) false
  | 6 -> dessiner_pi�ce6 (x, y) (num�ro mod 2 = 0)
  | 7 -> dessiner_pi�ce7 (x, y) (num�ro mod 2 = 0)
  | _ -> ();;

let dessiner_s�lection (x, y) =
  set_color foreground;
  set_line_width 3;
  draw_rect x y taille_pi�ce taille_pi�ce;
  draw_rect x y taille_pi�ce taille_pi�ce;
  set_line_width 1;;

let dessiner_d�s�lection (x, y) =
  set_color background;
  set_line_width 3;
  draw_rect x y taille_pi�ce taille_pi�ce;
  set_line_width 1;
  set_color foreground;
  draw_rect x y taille_pi�ce taille_pi�ce;;

let espace_pi�ces = 8;;

let largeur_zone_de_jeu () =
  �tat.zone_de_jeu.colonnes * taille_pi�ce +
  (�tat.zone_de_jeu.colonnes - 1) * espace_pi�ces

and hauteur_zone_de_jeu () =
  �tat.zone_de_jeu.lignes * taille_pi�ce +
  (�tat.zone_de_jeu.lignes - 1) * espace_pi�ces;;

let largeur_caract�re = 11
and hauteur_caract�re = 14;;

let largeur_cartouche =
  largeur_caract�re * (String.length " PAS DE CIRCUIT");;

let sizex = size_x () - 40
and sizey = size_y () - 40;;

let espace_horizontal () =
  (sizex - largeur_cartouche - largeur_zone_de_jeu ()) / 2;;

let espace_vertical () =
  (sizey - hauteur_zone_de_jeu ()) / 2;;

let dessiner_zone_de_jeu () =
  let x = ref (10 + espace_horizontal ())
  and y = ref (10 + espace_vertical ()) in
  (* o� placer le coin en bas � gauche de la zone de jeu *)
   for c = �tat.zone_de_jeu.d�butc to �tat.zone_de_jeu.finc do
     for l = �tat.zone_de_jeu.d�butl to �tat.zone_de_jeu.finl do
       begin match �tat.zone_de_jeu.tableau.(c).(l) with
       | Vide -> ()
       | Occup�e couleur -> dessiner_pi�ce (!x, !y) couleur
       end;
       y := !y + taille_pi�ce + espace_pi�ces;
    done;
    y := 10 + espace_vertical ();
    x := !x + taille_pi�ce + espace_pi�ces
  done;;

let dessiner_cartouche () =
  let gauche_cartouche =
    sizex - largeur_cartouche + largeur_caract�re in
  set_color foreground;
  set_font "Courier";
  set_text_size 14;
  moveto (sizex - largeur_cartouche) 20;
  rlineto 0 (sizey - 20);
  moveto gauche_cartouche (20 + sizey / 3);
  draw_string "NIVEAU";
  moveto sizex (20 + sizey / 3);
  draw_string (string_of_int �tat.niveau);;

let coordonn�es_centre_pi�ce (c, l) =
  (10 + espace_horizontal () + (c - 1) * taille_pi�ce +
   (c - 1) * espace_pi�ces + taille_pi�ce / 2,
   10 + espace_vertical () + (l - 1) * taille_pi�ce +
   (l - 1) * espace_pi�ces + taille_pi�ce / 2);;

let dessiner_circuit circuit =
  match List.map coordonn�es_centre_pi�ce circuit with
  | [] -> ()
  | (x, y) :: circuit' ->
    set_line_width 3;
    moveto x y;
    List.iter (function (x, y) -> lineto x y) circuit';
    set_line_width 1;;

let coordonn�es_pi�ce (c, l) =
  (10 + espace_horizontal () + (c - 1) * taille_pi�ce + (c - 1) * espace_pi�ces,
   10 + espace_vertical () + (l - 1) * taille_pi�ce + (l - 1) * espace_pi�ces);;

let effacer_pi�ce_s�lectionn�e (c, l) =
  let (x, y) = coordonn�es_pi�ce (c, l) in
  set_color white;
  fill_rect x y taille_pi�ce taille_pi�ce;
  set_line_width 3;
  draw_rect x y taille_pi�ce taille_pi�ce;
  draw_rect x y taille_pi�ce taille_pi�ce;
  set_line_width 1;
  set_color foreground;;

let effacer_circuit circuit =
  set_color background;
  dessiner_circuit circuit;
  set_color foreground;;

let �crire_pas_de_circuit () =
  let gauche_cartouche = sizex - largeur_cartouche + largeur_caract�re in
  let affiche () =
    set_color foreground;
    set_font "Courier-Bold";
    set_text_size 18;
    moveto gauche_cartouche (20 + 2 * sizey / 3);
    draw_string "PAS DE CIRCUIT" in
  affiche ();;

let position_zone_de_jeu (x, y) =
  ((x - espace_horizontal ()) / (taille_pi�ce + espace_pi�ces) + bord_gauche,
   (y - espace_vertical ()) / (taille_pi�ce + espace_pi�ces) + bord_bas);;

let position_pi�ce_zone_de_jeu_valide (c, l) =
  c >= �tat.zone_de_jeu.d�butc && c <= �tat.zone_de_jeu.finc &&
  l >= �tat.zone_de_jeu.d�butl && l <= �tat.zone_de_jeu.finl &&
  �tat.zone_de_jeu.tableau.(c).(l) <> Vide ;;

exception Non_vide;;

let zone_de_jeu_vide () =
  try
    for c = �tat.zone_de_jeu.d�butc to �tat.zone_de_jeu.finc do
      for l = �tat.zone_de_jeu.d�butl to �tat.zone_de_jeu.finl do
        if �tat.zone_de_jeu.tableau.(c).(l) <> Vide then raise Non_vide;
      done;
    done;
    true
  with Non_vide -> false;;

let s�lectionner (c, l) =
  �tat.s�lection <- Some (c, l);
  dessiner_s�lection (coordonn�es_pi�ce (c, l));;

let �liminer (c, l) (c', l') =
  �tat.s�lection <- None;
  (* supprimer le couple de pi�ces graphiquement *)
  effacer_pi�ce_s�lectionn�e (c, l);
  effacer_pi�ce_s�lectionn�e (c', l');
  (* supprimer le couple de pi�ces en m�moire *)
  �tat.zone_de_jeu.tableau.(c).(l) <- Vide;
  �tat.zone_de_jeu.tableau.(c').(l') <- Vide;;

exception Circuit of (int * int) * (int * int);;

let d�tection_circuits_valides cg lg =
  try
    for c = max cg �tat.zone_de_jeu.d�butc to �tat.zone_de_jeu.finc do
      for l = max lg �tat.zone_de_jeu.d�butl to �tat.zone_de_jeu.finl do
        for c' = max cg �tat.zone_de_jeu.d�butc to �tat.zone_de_jeu.finc do
          for l' = max lg �tat.zone_de_jeu.d�butl to �tat.zone_de_jeu.finl do
            if (c, l) <> (c', l') &&
              (circuit_valide (c, l) (c', l')) <> []
            then raise (Circuit ((c, l), (c', l')))
          done;
        done;
      done;
    done;
    raise Not_found
  with Circuit ((c, l), (c', l')) -> ((c, l), (c', l'));;

let dur�e_niveau =  [| 56.0; 72.0; 96.0; 112.0; 128.0; 144.0 |];;

let dessiner_barre_progression () =
  let ld = largeur_cartouche - 2 * largeur_caract�re in
  let gauche_cartouche =
    size_x () - 30 - largeur_cartouche + largeur_caract�re in
  draw_rect gauche_cartouche taille_pi�ce ld taille_pi�ce;;

let mise_�_jour_barre_progression t =
  let d = dur�e_niveau.(�tat.niveau) in
  let ld = largeur_cartouche - 2 * largeur_caract�re in
  let lt = int_of_float (t /. d *. float_of_int ld) in
  let gauche_cartouche =
    size_x () - 30  - largeur_cartouche + largeur_caract�re in
  fill_rect gauche_cartouche taille_pi�ce lt taille_pi�ce;;

let �crire_centr�_fen�tre haut s =
  let (w, h)= text_size s in
  moveto ((size_x () - w) / 2) (haut - h / 2);
  draw_string s;;

let limited_read_string len = "alleluia";;

let lecture_boite s (nom_police_texte, taille_police_texte)
  l (nom_police_lecture, taille_police_lecture) =
  set_font nom_police_texte;
  set_text_size taille_police_texte;
  let (w, h) = text_size s in
  set_font nom_police_lecture;
  set_text_size taille_police_lecture;
  let (w1, hc) = text_size "M" in
  let wl = l * w1 in
  let lw = (size_x () - (w + wl)) / 2 in
  moveto lw (size_y () / 2 - h / 2);
  set_font nom_police_texte;
  set_text_size taille_police_texte;
  draw_string s;
  draw_rect (current_x () + w1) (current_y () - hc / 2) (wl + 2 * w1) (2 * hc);
  rmoveto (2 * w1) 0;
  set_font nom_police_lecture;
  set_text_size taille_police_lecture;
  limited_read_string l;;

let lecture_nom () =
  clear_graph ();
  set_font "Helvetica-Bold";
  set_text_size 18;
  set_color foreground;
  �crire_centr�_fen�tre (size_y () * 4 / 5) "BRAVO";
  set_font "Helvetica-Bold";
  set_text_size 18;
  �crire_centr�_fen�tre (size_y () * 2 / 3)
   "Vous faites partie des dix meilleurs scores";
  let nom =
    lecture_boite ("Entrez votre nom? (" ^ Sys.getenv "USER" ^ ") :")
      ("Helvetica-Bold", 18) 20
      ("Courier", 18) in
  if nom = "" then Sys.getenv "USER" else nom;;

let mise_�_jour_tableau_scores tableau_scores =
  let len = Array.length tableau_scores in
  if �tat.score > snd tableau_scores.(len - 1) then
    begin
      let i = ref 0 in
      while !i < len && snd tableau_scores.(!i) >= �tat.score do
        incr i
      done;
      for j = len - 2 downto !i do
        tableau_scores.(j + 1) <- tableau_scores.(j)
      done;
      tableau_scores.(!i) <- (lecture_nom (), �tat.score)
    end;;

let affichage_meilleurs_scores tableau_scores =
  clear_graph ();
  set_font "Courier-Bold";
  set_text_size 18;
  set_color foreground;
  �crire_centr�_fen�tre (size_y () * 9 / 10) "Meilleurs scores";
  let nom_le_plus_long = ref "" in
  for i = 0 to Array.length tableau_scores - 1 do
    if String.length (!nom_le_plus_long) <
       String.length (fst tableau_scores.(i))
    then nom_le_plus_long := fst tableau_scores.(i)
  done;
  let longueur_nom = fst (text_size !nom_le_plus_long) in
  let longueur_score =
    fst (text_size (string_of_int (snd tableau_scores.(0)))) in
  let longueur_a�ration = fst (text_size "                      ") in
  let longueur_totale = longueur_nom + longueur_a�ration + longueur_score in
  let g = (size_x () - longueur_totale) / 2 in
  let dmin = g + longueur_score + longueur_a�ration in
  Array.iteri
    (function i -> function (nom, score) ->
       moveto g (size_y () * 2 / 3 - size_y () * i / 20);
       draw_string nom;
       let d = dmin + longueur_score - fst (text_size (string_of_int score)) in
       moveto d (size_y () * 2 / 3 - size_y () * i / 20);
       draw_string (string_of_int score))
    tableau_scores;;

let lire_fichier_meilleurs_scores () =
  try
    let d = open_in_bin "/tmp/scores" in
    let t = input_value d in
    close_in d;
    t
  with
  | End_of_file | Sys_error _ -> Array.make 10 ("", 0);;

let �crire_fichier_meilleurs_scores tableau_scores =
  let d = open_out_bin "/tmp/scores" in
  output_value d tableau_scores;
  close_out d;;

let attente_touche () =
  �crire_centr�_fen�tre 10 "APPUYER SUR UNE TOUCHE POUR CONTINUER";;

let jeu () =
  Random.self_init ();
  �tat.s�lection <- None;
  �tat.niveau <- 4;
  draw_rect 20 20 sizex sizey;
  set_color (rgb 100 100 100);
  fill_rect 20 sizey sizex 20;
  set_font "Helvetica-Bold";
  set_text_size 10;
  (*set_font
    "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso8859-1";*)
  set_color white;
  moveto 30 (sizey + 3);
  draw_string "Court circuit";
  set_color black;
  nouvelle_zone_de_jeu ();
  dessiner_zone_de_jeu ();
  dessiner_cartouche ();
  dessiner_barre_progression ();
  mise_�_jour_barre_progression (dur�e_niveau.(�tat.niveau) /. 3.0);
  �tat.score <- 6210;
  let tableau_scores = lire_fichier_meilleurs_scores () in
  mise_�_jour_tableau_scores tableau_scores;
  �crire_fichier_meilleurs_scores tableau_scores;
  affichage_meilleurs_scores tableau_scores;
  attente_touche ();
  exit 0;;

jeu ();;
