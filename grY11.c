/***********************************************************************/
/*                                                                     */
/*                           Active dvi                                */
/*                                                                     */
/*            Jun Furuse, Pierre Weis, Didier R�my                     */
/*            projet Cristal, INRIA Rocquencourt                       */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <memory.h>
#include <alloc.h>
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

value gr_sync(void)
{
  gr_check_open();
  XSync(grdisplay, 0);
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

/* may not be correct ? may use the output of xwininfo ? */
void get_position_against_root( Window w, int *pos )
{
  Window root, parent;
  Window *children;
  int nchildren;
  XWindowAttributes attr;
    
  XGetWindowAttributes(grdisplay, w, &attr);
  pos[0] += attr.x;
  pos[1] += attr.y;
  XQueryTree(grdisplay, w, &root, &parent, &children, &nchildren);
  if(children != NULL){
    XFree(children);
  }
  if( root == parent ){ 
    return; 
  } else {
    get_position_against_root( parent, pos );
  }
}

value gr_get_geometry(value unit){
  CAMLparam1(unit);
  CAMLlocal1(res);
  XWindowAttributes attr;
  int pos[2] = {0,0};

  XGetWindowAttributes(grdisplay, grwindow.win, &attr);
  get_position_against_root( grwindow.win, pos );

  res = alloc_tuple(4);
  Field(res,0) = Val_int(attr.width);
  Field(res,1) = Val_int(attr.height);
  Field(res,2) = Val_int(pos[0]);
  Field(res,3) = Val_int(pos[1]);
  CAMLreturn(res);
}

/* get modifiers... */
value gr_get_modifiers(void)
{
  int mouse_x, mouse_y, button, key, keypressed;
  Window rootwin, childwin;
  int root_x, root_y, win_x, win_y;
  unsigned int modifiers;
  unsigned int i;

  if (XQueryPointer(grdisplay, grwindow.win,
                    &rootwin, &childwin,
                    &root_x, &root_y, &win_x, &win_y,
                    &modifiers)) {
    button = 0;
    if (modifiers & Button1Mask) button = button | 0x1;
    if (modifiers & Button2Mask) button = button | 0x2;
    if (modifiers & Button3Mask) button = button | 0x4;
    if (modifiers & Button4Mask) button = button | 0x8;
    if (modifiers & Button5Mask) button = button | 0x10;
    
    if (modifiers & ShiftMask) button = button | 0x100;
    if (modifiers & ControlMask) button = button | 0x200;
    if (modifiers & Mod1Mask) button = button | 0x400;
    if (modifiers & Mod2Mask) button = button | 0x800;
    if (modifiers & Mod3Mask) button = button | 0x1000;
  } else {
    button = -1;
  }
  return Val_int(button);
}

/* Sub windows */
/*** Still in the graphics library
value gr_open_subwindow(value x, value y, value width, value height)
{
  Window win;

  gr_check_open();
  win = XCreateSimpleWindow(grdisplay, grwindow.win,
                            Int_val(x), Int_val(y), 
                            Int_val(width), Int_val(height),
                            0, grblack, grbackground);
  XMapWindow(grdisplay, win);
  XFlush(grdisplay);
  return (id_of_window ( win ));
}

value gr_close_subwindow(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XDestroyWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}
***/

value gr_map_window(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XMapWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_unmap_window(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XUnmapWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}

value gr_move_window (value wid, value x, value y)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XMoveWindow(grdisplay, win, Int_val(x), Int_val(y));
  XFlush(grdisplay);
  return Val_unit;
}

value gr_resize_window (value wid, value w, value h)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XResizeWindow(grdisplay, win, Int_val(w), Int_val(h));
  XFlush(grdisplay);
  return Val_unit;
}
