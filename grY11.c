/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <fail.h>
#include "libgraph.h"


value gr_bsize_x(void)
{
  gr_check_open();
  return Val_int(grbstore.w);
}

value gr_bsize_y(void)
{
  gr_check_open();
  return Val_int(grbstore.h);
}

value gr_screen_x(void)
{
  XWindowAttributes att;
  gr_check_open();
  XGetWindowAttributes (grdisplay, DefaultRootWindow(grdisplay), &att); 
  return Val_int(att.width);
}

value gr_screen_y(void)
{
  XWindowAttributes att;
  gr_check_open();
  XGetWindowAttributes (grdisplay, DefaultRootWindow(grdisplay), &att); 
  return Val_int(att.height);
}

value gr_bstore(void)
{
  gr_check_open();
  return id_of_window(grbstore.win);
}

value gr_flush(void)
{
  gr_check_open();
  XFlush(grdisplay);
  return Val_unit ;
}

value gr_set_named_atom_property (value name, value string) {
  Atom a = XInternAtom (grdisplay, String_val(name), 0);
  XChangeProperty (grdisplay, grwindow.win, 
                   a,       			/* property */ 
                   XA_STRING,                   /* xa_string */ 
                   8,       			/* format */
                   PropModeReplace, 		/* mode */
                   String_val(string), 		/* data */
                   string_length (string) 	/* nelements */
                   );
  XSync(grdisplay, False);
  return Val_unit;
}

value gr_set_cursor(value glyphid) {
  Cursor c;
  int gid;
  gid = Int_val(glyphid);
  if (gid < 0 || gid >= XC_num_glyphs) {
    invalid_argument("set_cursor");
  }
  c = XCreateFontCursor(grdisplay, gid);
  XDefineCursor(grdisplay, grwindow.win, c);
  XSync(grdisplay, False);
  return Val_unit;
}

value gr_unset_cursor(value unit) {
  XUndefineCursor(grdisplay, grwindow.win);
  XSync(grdisplay, False);
  return Val_unit;
}

