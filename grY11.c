/***********************************************************************/
/*                                                                     */
/*                           Active dvi                                */
/*                                                                     */
/*            Jun Furuse, Pierre Weis, Didier Rémy                     */
/*            projet Cristal, INRIA Rocquencourt                       */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <caml/memory.h>
#include <caml/alloc.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <caml/fail.h>
#include "libgraph.h"
#include "image.h"

value gr_get_color(void)
{
  return Val_int(grcolor);
}

value gr_bsize_x(void)
{
  gr_check_open();
  return Val_int(grbstore.w);
}

value gr_draw_area(value im, value xywh, value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  int wy = Wcvt(y) + 1 - Height_im(im);
  int by = Bcvt(y) + 1 - Height_im(im);
  int width = Int_val(Field (xywh, 2));
  int height = Int_val(Field (xywh, 3));
  int dx = Int_val(Field (xywh, 0));
  int dy = Int_val(Field (xywh, 1));

  gr_check_open();
  if (width > Width_im(im) - dx) width = Width_im(im) - dx;
  if (height > Height_im(im) - dy) height = Width_im(im) - dy;
  if (Mask_im(im) != None) {
    if(grremember_mode) {
      XSetClipOrigin(grdisplay, grbstore.gc, x, by);
      XSetClipMask(grdisplay, grbstore.gc, Mask_im(im));
    }
    if(grdisplay_mode) {
      XSetClipOrigin(grdisplay, grwindow.gc, x, wy);
      XSetClipMask(grdisplay, grwindow.gc, Mask_im(im));
    }
  }
  if(grremember_mode)
    XCopyArea(grdisplay, Data_im(im), grbstore.win, grbstore.gc,
              dx, dy,
              width, height,
              x, by);
  if(grdisplay_mode)
    XCopyArea(grdisplay, Data_im(im), grwindow.win, grwindow.gc,
          dx, dy,
          width, height,
          x, wy);
  if (Mask_im(im) != None) {
    if(grremember_mode)
      XSetClipMask(grdisplay, grbstore.gc, None);
    if(grdisplay_mode)
      XSetClipMask(grdisplay, grwindow.gc, None);
  }
  if(grdisplay_mode)
    XFlush(grdisplay);
  return Val_unit;
}

value gr_anti_synchronize(void)
{
  gr_check_open();
  XCopyArea(grdisplay, grwindow.win, grbstore.win, grwindow.gc,
            0, grbstore.h - grwindow.h,
            grwindow.w, grwindow.h,
            0, 0);
  XFlush(grdisplay);
  return Val_unit ;
}

value gr_window_color(value vx, value vy)
{
  int x = Int_val(vx);
  int y = Int_val(vy);
  XImage * im;
  int rgb;

  gr_check_open();
  im = XGetImage(grdisplay, grwindow.win, x, Bcvt(y), 1, 1, (-1), ZPixmap);
  rgb = gr_rgb_pixel(XGetPixel(im, 0, 0));
  XDestroyImage(im);
  return Val_int(rgb);
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

void gr_origin(int* x, int* y)
{
  Window win, root, parent, r;
  Window* children;
  int dx, dy, h, w, b, d; 
  gr_check_open();
  win = grwindow.win;
  root = DefaultRootWindow(grdisplay);
  *x = 0; *y = 0;
  while (win != root) {
    XGetGeometry (grdisplay, win, &r, &dx, &dy, &w, &h, &b, &d);
    *x = *x + dx;
    *y = *y + dy;
    XQueryTree (grdisplay, win, &r, &parent, &children, &b);
    win = parent;
  }
  return;
}

value gr_origin_x(void)
{
  int x, y;
  gr_origin (&x, &y); 
  return Val_int(x);
}
value gr_origin_y(void)
{
  int x, y;
  gr_origin (&x, &y); 
  return Val_int(y);
}

value gr_window(void)
{ unsigned int w; value res;
  gr_check_open();
  w = grwindow.win;
  return copy_int32 (w); 
}

value gr_bstore(void)
{ unsigned int w; value res;
  gr_check_open();
  w = grbstore.win;
  return copy_int32 (w); 
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

/* The following is not the best, since it unsets the selection 
   It would be better to own the selection. However, the event loop should
   then be changed */
value gr_cut (value string) {
  /* The following, suggested by Fabrice does not work */
  /* 
  Atom cut_buffers[] = 
     { XA_CUT_BUFFER0,
       XA_CUT_BUFFER1, 
       XA_CUT_BUFFER2, 
       XA_CUT_BUFFER3, 
       XA_CUT_BUFFER4, 
       XA_CUT_BUFFER5, 
       XA_CUT_BUFFER6, 
       XA_CUT_BUFFER7
     };
  XRotateWindowProperties (grdisplay, grwindow.win,
                           cut_buffers, 8, 1);
  */
  /* XRotateBuffers(grdisplay, 1); */
  /*
  XChangeProperty (grdisplay, grwindow.win, 
                   XA_CUT_BUFFER0,	      
                   XA_STRING,                  
                   8,       			
                   PropModeReplace, 		
                   String_val (string),
                   String_length (string)
                   );
  */
  XStoreBytes (grdisplay, String_val (string), string_length (string)); 
  XSetSelectionOwner (grdisplay,
                      XA_PRIMARY,
                      None,
                      CurrentTime);
  XSync (grdisplay, False);
  return Val_unit; 
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
value gr_open_sub_window(value vx, value vy, value width, value height)
{
  Window win;

  int h = Int_val(height);
  int w = Int_val(width);
  int x = Int_val(vx);
  int y = Int_val(vy);

  gr_check_open();
  win = XCreateSimpleWindow(grdisplay, grwindow.win,
                            x, Wcvt(y + h), 
                            w, h,
                            0, grblack, grbackground);
  XMapWindow(grdisplay, win);
  XFlush(grdisplay);

  return (id_of_window(win));
}

/* In graphics */
value gr_close_subwindow2(value wid)
{
  Window win;

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XDestroyWindow(grdisplay, win);
  XFlush(grdisplay);
  return Val_unit;
}

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

value gr_move_window (value wid, value grx, value gry, value height)
{
  Window win;

  int x = Int_val(grx);
  int y = Int_val(gry);
  int h = Int_val(height);

  gr_check_open();
  sscanf( String_val(wid), "%lu", (unsigned long *)(&win) );
  XMoveWindow(grdisplay, win, x, Wcvt(y + h));
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

value gr_reposition (value x, value y, value w, value h)
{
  Window win;
  XWindowAttributes att;
  int width, height;

  gr_check_open();
  width = Int_val(w); height = Int_val(h);
  if (width < 0) { 
    XGetWindowAttributes (grdisplay, DefaultRootWindow(grdisplay), &att);
    width = att.width; height = att.height;
  }; 
  XMoveResizeWindow(grdisplay, grwindow.win, 
                    Int_val(x), Int_val(y), width, height);
  /* Not sufficient, should tell the manager not to decorate the window */
  XSetWindowBorderWidth(grdisplay, grwindow.win, width<0 ? 0 : BORDER_WIDTH);

    grwindow.w = width;
    grwindow.h = height;
    if (grwindow.w > grbstore.w || grwindow.h > grbstore.h) {

      /* Allocate a new backing store large enough to accomodate
         both the old backing store and the current window. */
      struct canvas newbstore;
      newbstore.w = max(grwindow.w, grbstore.w);
      newbstore.h = max(grwindow.h, grbstore.h);
      newbstore.win =
        XCreatePixmap(grdisplay, grwindow.win, newbstore.w, newbstore.h,
                      XDefaultDepth(grdisplay, grscreen));
      newbstore.gc = XCreateGC(grdisplay, newbstore.win, 0, NULL);
      XSetBackground(grdisplay, newbstore.gc, grwhite);
      XSetForeground(grdisplay, newbstore.gc, grwhite);
      XFillRectangle(grdisplay, newbstore.win, newbstore.gc,
                     0, 0, newbstore.w, newbstore.h);
      XSetForeground(grdisplay, newbstore.gc, grcolor);
      if (grfont != NULL)
        XSetFont(grdisplay, newbstore.gc, grfont->fid);

      /* Copy the old backing store into the new one */
      XCopyArea(grdisplay, grbstore.win, newbstore.win, newbstore.gc,
                0, 0, grbstore.w, grbstore.h, 0, newbstore.h - grbstore.h);

      /* Free the old backing store */
      XFreeGC(grdisplay, grbstore.gc);
      XFreePixmap(grdisplay, grbstore.win);

      /* Use the new backing store */
      grbstore = newbstore;
    }

  XFlush(grdisplay);
  return Val_unit;
}
