(*
 * advi - A DVI previewer
 * Copyright (C) 2000  Alexandre Miquel
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Lesser General Public License version 2.1 for more
 * details (enclosed in the file LGPL).
 *)


exception Pause
type cooked_dvi
val cook_dvi : Dvi.t -> cooked_dvi
val render_page : cooked_dvi -> int -> float -> int -> int -> unit
val render_step : cooked_dvi -> int -> float -> int -> int -> (unit -> bool)
val unfreeze_fonts : cooked_dvi -> unit
val unfreeze_glyphs : cooked_dvi -> float -> unit
val scan_specials : cooked_dvi -> int -> unit
val clear_symbols : unit -> unit

