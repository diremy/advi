(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier R�my and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

open Tk;;

let taquin_color = ref (NamedColor "white");;

let filename = ref "";;

let opencamltk () =
 Arg.parse keywords (fun s -> filename := s)
  "Specify an image file name to use";
 opentk_with_parsed_args ();;

let d�coupe_image img nx ny =
  let l = Imagephoto.width img
  and h = Imagephoto.height img in
  let tx = l / nx and ty = h / ny in
  let pi�ces = ref [] in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      let pi�ce = Imagephoto.create
                    [Width (Pixels tx); Height (Pixels ty)] in
      Imagephoto.copy pi�ce img
        [ImgFrom(x * tx, y * ty, (x + 1) * tx, (y + 1) * ty)];
      pi�ces := pi�ce :: !pi�ces
    done
  done;
  (tx, ty, List.tl !pi�ces);;

let remplir_taquin c nx ny tx ty pi�ces =
  let trou_x = ref (nx - 1)
  and trou_y = ref (ny - 1) in
  let trou =
    Canvas.create_rectangle c
      (Pixels (!trou_x * tx)) (Pixels (!trou_y * ty))
      (Pixels tx) (Pixels ty) [] in
  let taquin = Array.make_matrix nx ny trou in
  let p = ref pi�ces in
  for x = 0 to nx - 1 do
    for y = 0 to ny - 1 do
      match !p with
      | [] -> ()
      | pi�ce :: reste ->
          taquin.(x).(y) <-
            Canvas.create_image c
                (Pixels (x * tx)) (Pixels (y * ty))
                [ImagePhoto pi�ce; Anchor NW; Tags [Tag "pi�ce"]];
          p := reste
    done
  done;
  let d�placer x y =
    let pi�ce = taquin.(x).(y) in
    Canvas.coords_set c pi�ce
      [Pixels (!trou_x * tx); Pixels(!trou_y * ty)];
    Canvas.coords_set c trou
      [Pixels (x * tx); Pixels(y * ty); Pixels tx; Pixels ty];
    taquin.(!trou_x).(!trou_y) <- pi�ce;
    taquin.(x).(y) <- trou;
    trou_x := x; trou_y := y in
  let jouer ei =
    let x = ei.ev_MouseX / tx and y = ei.ev_MouseY / ty in
    if x = !trou_x && (y = !trou_y - 1 || y = !trou_y + 1)
    || y = !trou_y && (x = !trou_x - 1 || x = !trou_x + 1)
    then d�placer x y in
  Canvas.bind c (Tag "pi�ce") [[], ButtonPress]
                 (BindSet ([Ev_MouseX; Ev_MouseY], jouer));;

let rec permutation = function
  | [] -> []
  | l  -> let n = Random.int (List.length l) in
          let (�l�ment, reste) = partage l n in
          �l�ment :: permutation reste

and partage l n =
  match l with
  | [] -> failwith "partage"
  | t�te :: reste ->
      if n = 0 then (t�te, reste) else
        let (�l�ment, reste') = partage reste (n - 1) in
        (�l�ment, t�te :: reste');;

let create_filled_text parent lines =
  let lnum = List.length lines
  and lwidth =
    List.fold_right
     (fun line max ->
       let l = String.length line in
       if l > max then l else max)
     lines 1 in
  let txtw = Text.create parent [TextWidth lwidth; TextHeight lnum] in
  List.iter
   (fun line ->
     Text.insert txtw (TextIndex (End, [])) line [];
     Text.insert txtw (TextIndex (End, [])) "\n" [])
   lines;
  txtw;;

let give_help parent lines () =
 let help_window = Toplevel.create parent [] in
 Wm.title_set help_window "Help";

 let help_frame = Frame.create help_window [] in

 let help_txtw = create_filled_text help_frame lines in

 let quit_help () = destroy help_window in
 let ok_button = Button.create help_frame [Text "Ok"; Command quit_help] in

 pack [help_txtw; ok_button ] [Side Side_Bottom];
 pack [help_frame] [];;

let taquin nx ny =
  let fp = opencamltk () in
  Wm.title_set fp "Taquin";
  Toplevel.configure fp [Background !taquin_color];
  let img = Imagephoto.create [File !filename] in
  let c =
    Canvas.create fp
     [Width(Pixels(Imagephoto.width img));
      Height(Pixels(Imagephoto.height img));
      Background !taquin_color] in
  let (tx, ty, pi�ces) = d�coupe_image img nx ny in
  remplir_taquin c nx ny tx ty (permutation pi�ces);
  pack [c] [];

  let quit =
    Button.create fp
      [Text "Quit"; Background !taquin_color; Command closeTk] in
  let help_lines =
   ["Pour jouer, cliquer sur une des pi�ces";
    "entourant le trou";
    "";
    "To play, click on a part around the hole"] in
  let help =
    Button.create fp
      [Text "Help"; Background !taquin_color;
       Command (give_help fp help_lines)] in
  pack [quit; help] [Side Side_Left; Fill Fill_X];
  mainLoop ();;

taquin 2 3;;
