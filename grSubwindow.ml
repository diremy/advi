exception Error of string

class t (fix : GPack.fixed) = object
  method create ~x ~y ~width ~height =
    if width = 0 && height = 0 then 
      raise (Error "subwindow#create: zero size?")
    else
      let w = GBin.event_box ~width ~height ~show: true () in
      fix#put (w :> GObj.widget) ~x ~y;
      w

  method destroy (emb : GBin.event_box) =
    fix#remove (emb :> GObj.widget);
    emb#destroy ()

  method move (emb : GBin.event_box) = fix#move (emb :> GObj.widget)
  method resize (emb : GBin.event_box) ~width ~height =
    emb#misc#set_geometry ~width ~height ()
end

