(* this file is used for preprocessing with camlp4 *)
open Pa_ifdef

let _ =
  if true then define "HAVE_GS";
  if true then define "HAVE_CAMLIMAGES"
;;
